with Marlowe.Btree_Keys;                use Marlowe.Btree_Keys;
with Marlowe.Btree_Handles;

procedure Test_Header_Overflow is

   Key_Description : constant Key_Component :=
     Make_Component (String_Key, 14);

   Handle : Marlowe.Btree_Handles.Btree_Handle;

begin

   Marlowe.Btree_Handles.Create (Handle, "test_overflow.key");

   for I in 1 .. 1_000 loop
      declare
         Name : constant String := "test" & Integer'Image (-I);
         Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
           Marlowe.Btree_Handles.Add_Key (Handle, Name,
                                          (1 => Key_Description));
      begin
         Marlowe.Btree_Handles.Insert (Handle, Ref,
                                       To_Btree_Key (Key_Description,
                                                     Name));
      end;
   end loop;

   Marlowe.Btree_Handles.Close (Handle);

   Marlowe.Btree_Handles.Open (Handle, "test_overflow.key");
   for I in 1 .. 1_000 loop
      declare
         Name : constant String := "test" & Integer'Image (-I);
         Ref : constant Marlowe.Btree_Handles.Btree_Reference :=
           Marlowe.Btree_Handles.Get_Reference (Handle, Name);
      begin
         Marlowe.Btree_Handles.Insert (Handle, Ref,
                                       To_Btree_Key (Key_Description,
                                                     Name & "'"));
      end;
   end loop;

   Marlowe.Btree_Handles.Close (Handle);

end Test_Header_Overflow;

