module QueryWithFragments = [%graphql
  {|
   query {
    users {
      id
      ... on AdminUser {
        name
      }
      ... on AnonymousUser {
        anonymousId
      }
    }
  }
|}]

type user = [
  | `User of < id : string >
  | `AdminUser of < id : string; name: string >
  | `AnonymousUser of < id : string; anonymousId : int >
]

type only_user = [
  | `User of < id : string >
]

module QueryWithoutFragments = [%graphql
  {|
   query {
    users {
      id
    }
  }
|}]

let json = {|{
             "users": [
             { "__typename": "AdminUser", "id": "1", "name": "bob" },
             { "__typename": "AnonymousUser", "id": "2", "anonymousId": 1},
             { "__typename": "OtherUser", "id": "3"}
             ]}|}

let user = (
  module struct
    type t = user

    let pp formatter = function
      | `User u -> Format.fprintf formatter "`User < id = @[%s@] >" u#id
      | `AdminUser u -> Format.fprintf formatter "`AdminUser < id = @[%s@]; name = @[%s@] >" u#id u#name
      | `AnonymousUser u -> Format.fprintf formatter "`AnonymousUser < id = @[%s@]; anonymousId = @[%i@] >" u#id u#anonymousId

    let equal (a: user) (b: user) = 
      match a, b with
      | (`User u1), (`User u2) -> u1#id = u2#id
      | (`AdminUser u1), (`AdminUser u2) -> u1#id = u2#id && u1#name = u2#name
      | (`AnonymousUser u1), (`AnonymousUser u2) -> u1#id = u2#id && u1#anonymousId = u2#anonymousId
      | _ -> false

  end : Alcotest.TESTABLE with type t = user)

let only_user = (
  module struct
    type t = only_user

    let pp formatter = function
      | `User u -> Format.fprintf formatter "`User < id = @[%s@] >" u#id

    let equal (a: only_user) (b: only_user) = 
      match a, b with
      | (`User u1), (`User u2) -> u1#id = u2#id

  end : Alcotest.TESTABLE with type t = only_user)

let decode_with_fragments () =
  Alcotest.(check (array user)) "query result equality"
    (QueryWithFragments.parse (Yojson.Basic.from_string json))#users
    [|
      `AdminUser (object method id = "1" method name = "bob" end);
      `AnonymousUser (object method id = "2" method anonymousId = 1 end);
      `User(object method id = "3" end);
    |]

let decode_without_fragments () =
  Alcotest.(check (array only_user)) "query result equality"
    (QueryWithoutFragments.parse (Yojson.Basic.from_string json))#users
    [|
      `User(object method id = "1" end);
      `User(object method id = "2" end);
      `User(object method id = "3" end);
    |]

let tests = [
  "Decodes the interface with fragments", `Quick, decode_with_fragments;
  "Decodes the interface without fragments", `Quick, decode_without_fragments;
]
