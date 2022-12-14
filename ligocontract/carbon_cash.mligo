type sicar_data = {
    sicar_id : string;
    latitude : string;
    longitude : string;
    app : nat;
    last_retification : timestamp
}

type storage = {
    contract_owner : address;
    minters : (address, bool) map ;
    wallet_properties : (address, (string, sicar_data) map) big_map;
    car_wallet : (string, address) big_map;
    balances : (address, nat) big_map;
    // is there anything like a bi-directional map?
    wallet_cnpj : (address, string) big_map;
    cnpj_wallet : (string, address) big_map;
    allowances : (address, (address, nat) map) big_map 
}

type return = operation list * storage

type transfer_args = {to : address; amount : nat}
type transfer_from_args = {from : address; to : address; amount : nat}
type approve_args = {spender: address; amount : nat}
type register_property_args = {wallet : address; info : sicar_data; amount : nat}
type register_company_args = {wallet : address; cnpj : string}
type spend_args = {wallet : address; amount : nat}

type function =
    Transfer of transfer_args
    | Transfer_from of transfer_from_args
    | Approve of approve_args
    | RegisterCar of register_property_args
    | Init_storage of address
    | Mint of string
    | Spend of spend_args
    | RegisterCompany of register_company_args
    | AddMinter of address
    | DelMinter of address

let transfer_core (from, to, amount, store : address * address * nat * storage) : return =
    let owner_balance =
        match Big_map.find_opt from store.balances with
        | Some k -> k
        | None -> (failwith "Caller does not have any balance" : nat) in
    let () = assert_with_error (owner_balance >= amount) "Caller does not have enough funds" in
    let updated_balance_map = Big_map.update from (Some( abs (owner_balance - amount))) store.balances in
    let to_balance =
        match Big_map.find_opt to updated_balance_map with
        | Some k -> k
        | None -> 0n in
    let updated_balance_map = Big_map.update to (Some( (to_balance) + amount)) updated_balance_map in
    let store : storage = {store with balances = updated_balance_map} in
    [], store

let transfer (args, store : transfer_args * storage) : return =
    let owner_address : address = Tezos.get_source () in
    transfer_core (owner_address, args.to, args.amount, store)

let update_allowances(owner, spender, owner_allowances, amount, store : address * address * (address, nat) map * nat * storage) : return =
    let updated_spender_allowances = Map.update spender (Some(amount)) owner_allowances in
    let updated_allowances = Big_map.update owner (Some(updated_spender_allowances)) store.allowances in
    let store : storage = {store with allowances = updated_allowances} in
    [], store

let transfer_from (args, store : transfer_from_args * storage) : return =
    let spender_address : address = Tezos.get_source () in
    let owner_allowances =
        match Big_map.find_opt args.from store.allowances with
        | Some k -> k
        | None -> (failwith "Owner does not allow anyone to spend their tokens" : (address, nat) map) in
    let spender_allowance =
        match Map.find_opt spender_address owner_allowances with
        | Some k -> if k >= args.amount then k else (failwith "Caller cannot spend this amount of tokens" : nat)
        | None -> (failwith "Owner does not allow this caller to spend any tokens" : nat) in
    let (_, store) = transfer_core (args.from, args.to, args.amount, store) in
    update_allowances (args.from, spender_address, owner_allowances, abs(spender_allowance - args.amount), store)

let approve (args, store : approve_args * storage) : return =
    let owner_address : address = Tezos.get_source () in
    let owner_allowances =
        match Big_map.find_opt owner_address store.allowances with
        | Some k -> k
        | None -> Map.empty in // CREATE THE MAP
    update_allowances (owner_address, args.spender, owner_allowances, args.amount, store)

let register_car (args, store : register_property_args * storage) : return =
    let caller_address : address = Tezos.get_source () in
    // for now, the caller must be the owner
    let () = assert_with_error (caller_address = store.contract_owner) "Only the owner can call this function" in
    let () = assert_with_error (String.length args.info.latitude < 25n) "Latitude must have less than 25 characters" in
    let () = assert_with_error (String.length args.info.longitude < 25n) "Longitude must have less than 25 characters" in
    // TODO: try to assert the sicar_id
    let _ = match Big_map.find_opt args.info.sicar_id store.car_wallet with
        | Some _ -> (failwith "Property is already registered to another wallet" : address)
        | None -> ("" : address) in
    let updated_car_wallet = Big_map.update args.info.sicar_id (Some(args.wallet)) store.car_wallet in
    let property_map : (string, sicar_data) map =
        match Big_map.find_opt args.wallet store.wallet_properties with
        | Some k -> k
        | None -> Map.empty in
    let updated_property_map = Map.add args.info.sicar_id args.info property_map in
    let updated_wallet_properties = Big_map.update args.wallet (Some(updated_property_map)) store.wallet_properties in
    let store : storage = {store with
        car_wallet = updated_car_wallet;
        wallet_properties = updated_wallet_properties} in
    [], store

let property_multiplier(last_retification : timestamp) : nat =
    let age = last_retification - (Tezos.get_now ()) in
    let one_year : int = 31536000 in
    if age < one_year then
        100n
    else if age < one_year * 3 then
        80n
    else if age < one_year * 5 then
        50n
    else
        15n

let add_minter (minter, store : address * storage) : return =
    let () = assert (Tezos.get_sender () = store.contract_owner) in
    let updated_minters = 
        match Map.find_opt minter store.minters with
        | Some _ -> (failwith "Minter is already added")
        | None -> Map.add minter true store.minters in
    let store = {store with minters = updated_minters} in
    [], store

let delete_minter (minter, store : address * storage) : return =
    let () = assert (Tezos.get_sender () = store.contract_owner) in
    let updated_minters = 
        match Map.find_opt minter store.minters with
        | Some _ -> Map.remove minter store.minters
        | None -> (failwith "Address given is not a minter") in
    let store = {store with minters = updated_minters} in
    [], store

let mint(car_identifier, store : string * storage) : return =
    //require(msg.sender == minter);
    //address wallet = carWallet[carIdentifier];
    //require(wallet != address(0));
    let caller_is_minter = Map.mem (Tezos.get_sender ()) store.minters in
    let () = assert (caller_is_minter) in
    let wallet =
        match Big_map.find_opt car_identifier store.car_wallet with
        | Some k -> k
        | None -> (failwith "This sicar identifier is not related to a wallet") in
    let property_option =
        match Big_map.find_opt wallet store.wallet_properties with
        | Some k -> Map.find_opt car_identifier k
        | None -> (failwith "There is inconsistencies between car_wallet and wallet_properties") in
    let property : sicar_data = Option.unopt property_option in
    let wallet_balance =
        match Big_map.find_opt wallet store.balances with
        | Some k -> k
        | None -> 0n in
    let minted_tokens = property.app * property_multiplier (property.last_retification) in
    let new_wallet_balance = wallet_balance + minted_tokens in
    let updated_balances = Big_map.update wallet (Some(new_wallet_balance)) store.balances in
    let store = {store with balances = updated_balances} in
    [], store

let spend (args, store : spend_args * storage) : return =
    let updated_balances =
        match Big_map.find_opt args.wallet store.balances with
        | Some k -> if k >= args.amount then Big_map.update args.wallet (Some(abs (k - args.amount))) store.balances else (failwith "Wallet does not have this balance to spend")
        | None -> (failwith "Wallet does not have any balance") in
    let store = {store with balances = updated_balances} in
    [], store

//update function if enable cnpj_wallet
let register_company (args, store : register_company_args * storage) : return =
    let _ =
        match Big_map.find_opt args.wallet store.wallet_cnpj with
        | Some _ -> (failwith "Property is already registered to another wallet" : address)
        | None -> ("" : address) in
    let updated_wallet_cnpj = Big_map.add args.wallet args.cnpj store.wallet_cnpj in
    let store = {store with wallet_cnpj = updated_wallet_cnpj} in
    [], store

// TODO: as i see in https://ligolang.org/docs/advanced/first-contract
// the initial storage is passed via command line.
// Therefore, this function may not be necessary
let init_storage (owner : address) : return =
    let store : storage = {
        contract_owner = owner;
        minters = Map.empty;
        wallet_properties = Big_map.empty;
        car_wallet = Big_map.empty;
        balances = Big_map.empty;
        wallet_cnpj = Big_map.empty;
        cnpj_wallet = Big_map.empty;
        allowances = Big_map.empty
    } in
    [], store

let main (function, store : function * storage) : return =
    match function with
        Transfer args -> transfer (args, store)
        | Transfer_from args -> transfer_from (args, store)
        | Approve args -> approve (args, store)
        | RegisterCar args -> register_car (args, store)
        | Init_storage owner -> init_storage (owner)
        | Mint car_identifier -> mint (car_identifier, store)
        | Spend args -> spend (args, store)
        | RegisterCompany args -> register_company (args, store)
        | AddMinter minter -> add_minter (minter, store)
        | DelMinter minter -> delete_minter (minter, store)