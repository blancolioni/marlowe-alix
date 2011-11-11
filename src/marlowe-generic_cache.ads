with Ada.Containers;

generic
   type Cached_Item is limited private;
   type Cached_Item_Address is private;
   with function Hash (Item : Cached_Item) return Ada.Containers.Hash_Type;
   with procedure Fetch (Location : Cached_Item_Address;
                         Item     : out Cached_Item);
   with procedure Store (Location : Cached_Item_Address;
                         Item     : Cached_Item);
package Marlowe.Generic_Cache is

   type Cached_Item_Info is private;

   type Marlowe_Cache is private;

   function Create_Cache (Maximum_Size : Natural)
                          return Marlowe_Cache;

   procedure Close (Cache : in out Marlowe_Cache);

   procedure Get_Item (From_Cache : in out Marlowe_Cache;
                       Location   : in     File_And_Page;
                       Result     :    out Cached_Item;
                       Info       :    out Cached_Item_Info);

   procedure Set_Dirty (Info : Cached_Item_Info);

   procedure Set_Accessed (Info : Cached_Item_Info);

   procedure Reference (Info : Cached_Item_Info);
   procedure Unreference (Info : Cached_Item_Info);

   procedure Flush (Cache : in out Marlowe_Cache);

   procedure Get_Cache_Statistics (Cache  : in     Marlowe_Cache;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural);

   procedure Shared_Lock    (Item : in Cached_Item_Info);
   procedure Update_Lock    (Item : in Cached_Item_Info);
   procedure Exclusive_Lock (Item : in Cached_Item_Info);

   procedure Upgrade_To_Exclusive (Item : in Cached_Item_Info);

   procedure Shared_Unlock (Item : in Cached_Item_Info);
   procedure Unlock (Item : in Cached_Item_Info);

   function Exclusive_Locked (Item : in Cached_Item_Info) return Boolean;
   function Update_Locked    (Item : in Cached_Item_Info) return Boolean;
   function Shared_Locked    (Item : in Cached_Item_Info) return Boolean;

private

   type Marlowe_Cache_Record;
   type Marlowe_Cache is access Marlowe_Cache_Record;

   type Cached_Item_Info_Record;
   type Cached_Item_Info is access Cached_Item_Info_Record;

end Marlowe.Generic_Cache;
