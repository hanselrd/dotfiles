{
  EnumRecord = fun enum_contract label value =>
    value
    |> std.record.to_array
    |> std.array.map
      (
        fun x =>
          {
            field =
              std.deep_seq
                (std.contract.apply enum_contract label (std.string.to_enum x.field))
                x.field,
            value = x.value
          }
      )
    |> std.record.from_array,
  insert : forall a b. [|; a |] -> b -> { _ : b } -> { _ : b } = fun field value enum_record =>
      std.record.insert
        (std.string.from_enum field)
        value
        enum_record
}
