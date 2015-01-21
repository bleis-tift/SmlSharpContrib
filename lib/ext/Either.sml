structure Either = struct
  open Std

  datatype ('a,'b) t =
    Left of 'a
    | Right of 'b

  fun mapLeft f x =
    case x of
         Left y => Left (f y)
       | Right y => Right y

  fun mapRight f x =
    case x of
         Left y => Left y
       | Right y => Right (f y)

  fun equal left right x =
    case x of
         (Left lhs, Left rhs) => left (lhs, rhs)
       | (Right lhs, Right rhs) => right (lhs, rhs)
       | _ => false

  fun isLeft x =
    case x of
         Left _ => true
       | Right _ => false

  fun isRight x =
    not $ isLeft x
end
