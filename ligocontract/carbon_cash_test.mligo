#include "carbon_cash.mligo"

let init_storage_with_owner (owner : address) : storage = 
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
    store

let init_storage_empty () : storage = 
    let store : storage = {
        contract_owner = ("tz1ZjYs5cJRGDhU5wzsriiYjWdu9fiMLmG3W" : address);
        minters = Map.empty;
        wallet_properties = Big_map.empty;
        car_wallet = Big_map.empty;
        balances = Big_map.empty;
        wallet_cnpj = Big_map.empty;
        cnpj_wallet = Big_map.empty;
        allowances = Big_map.empty
    } in
    store

let test = 
    let initial_storage : storage =  init_storage_empty () in
    let taddr, _, _ = Test.originate main initial_storage 0tez in
    let () = Test.log (Test.get_storage taddr) in
    assert (Test.get_storage taddr = initial_storage)

let test_approve = 
    let () = Test.reset_state 5n ([] : tez list) in
    let (owner, _pk, _sk) = Test.get_bootstrap_account (0n) in
    let () = Test.log owner in
    let initial_storage : storage =  init_storage_with_owner (owner) in
    let taddr, _, _ = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract taddr in
    let (spender, _, _) = Test.get_bootstrap_account (1n) in
    let allow_args = {
        spender = spender;
        amount = 50n
    } in
    let (caller, _, _) = Test.get_bootstrap_account (2n) in
    let () = Test.set_source caller in
    let _ = Test.transfer_to_contract_exn contr (Approve allow_args) 1mutez in
    let store : storage = Test.get_storage taddr in
    let owner_allowances : (address, nat) map = 
        match Big_map.find_opt caller store.allowances with
        | Some k -> k
        | None -> Test.failwith "Key does not exist" in
    let found : bool = Map.mem allow_args.spender owner_allowances in
    let () = assert (found = true) in
    let allowance : nat = Option.unopt(Map.find_opt allow_args.spender owner_allowances) in
    assert (allowance = 50n)

let test_allowance_spending = assert (true = true)

let test_transfer = 
    let () = Test.reset_state 5n ([] : tez list) in
    let (owner, _pk, _sk) = Test.get_bootstrap_account (0n) in
    let initial_storage : storage =  init_storage_with_owner (owner) in

    // mine some tokens for addresses
    let (acc1, _, _) = Test.get_bootstrap_account (1n) in
    let (acc2, _, _)= Test.get_bootstrap_account (2n) in
    let (acc3, _, _)= Test.get_bootstrap_account (3n) in
    let balances = Big_map.update acc1 (Some(50n)) initial_storage.balances in
    let balances = Big_map.update acc2 (Some(50n)) balances in
    let balances = Big_map.update acc3 (Some(50n)) balances in

    let initial_storage = {initial_storage with balances = balances} in
    let taddr, _, _ = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract taddr in

    let () = Test.set_source acc1 in
    let _ = Test.transfer_to_contract_exn contr (Transfer ({to = acc2; amount = 20n})) 1mutez in

    let store = Test.get_storage taddr in
    let () = assert ((Option.unopt (Big_map.find_opt acc1 store.balances)) = 30n) in
    let () = assert ((Option.unopt (Big_map.find_opt acc2 store.balances)) = 70n) in

    let _ = Test.transfer_to_contract contr (Transfer ({to = acc2; amount = 31n})) 1mutez in
    let new_store = Test.get_storage taddr in
    assert_with_error (store = new_store) "Storage should not have changed"

let test_transfer_from =
    let () = Test.reset_state 5n ([] : tez list) in
    let (owner, _pk, _sk) = Test.get_bootstrap_account (0n) in
    let initial_storage : storage =  init_storage_with_owner (owner) in

    // mine some tokens for addresses
    let (acc1, _, _) = Test.get_bootstrap_account (1n) in
    let (acc2, _, _)= Test.get_bootstrap_account (2n) in
    let (acc3, _, _)= Test.get_bootstrap_account (3n) in
    let balances = Big_map.update acc1 (Some(50n)) initial_storage.balances in
    let balances = Big_map.update acc2 (Some(50n)) balances in
    let balances = Big_map.update acc3 (Some(50n)) balances in

    let initial_storage = {initial_storage with balances = balances} in
    let taddr, _, _ = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract taddr in

    let () = Test.set_source acc1 in
    let _ = Test.transfer_to_contract_exn contr (Approve ({spender = acc2; amount = 30n})) 1mutez in

    let () = Test.set_source acc2 in
    let _ = Test.transfer_to_contract_exn contr (Transfer_from ({from = acc1; to = acc3; amount = 20n})) 1mutez in

    let store = Test.get_storage taddr in
    let () = assert ((Option.unopt (Big_map.find_opt acc1 store.balances)) = 30n) in
    let () = assert ((Option.unopt (Big_map.find_opt acc2 store.balances)) = 50n) in
    let () = assert ((Option.unopt (Big_map.find_opt acc3 store.balances)) = 70n) in

    let _ = Test.transfer_to_contract contr (Transfer_from ({from = acc1; to = acc3; amount = 20n})) 1mutez in
    let new_store = Test.get_storage taddr in
    assert_with_error (store = new_store) "Storage should not have changed"

let test_ = assert (true = true)

let test_mining = assert (true = true)