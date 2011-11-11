with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

with Marlowe.Files;
with Marlowe.Locks;
with Marlowe.Pages;

package Marlowe.Caches is

   type Cached_Page_Info is private;

   type File_Cache is private;

   function Create_Cache (File         : Marlowe.Files.File_Type;
                          Maximum_Size : Natural)
                         return File_Cache;

   procedure Close (Cache : in out File_Cache);

   procedure Get_Page (From_Cache : in out File_Cache;
                       Location   : in     File_And_Page;
                       Result     :    out Marlowe.Pages.Page;
                       Info       :    out Cached_Page_Info);

   procedure Set_Dirty (Info : Cached_Page_Info);

   procedure Set_Accessed (Info : Cached_Page_Info);

   procedure Reference (Info : Cached_Page_Info);
   procedure Unreference (Info : Cached_Page_Info);

   procedure Flush (Cache : in out File_Cache);

   procedure Get_Cache_Statistics (Cache  : in     File_Cache;
                                   Blocks :    out Natural;
                                   Pages  :    out Natural;
                                   Hits   :    out Natural;
                                   Misses :    out Natural);

   procedure Shared_Lock    (Item : in Cached_Page_Info);
   procedure Update_Lock    (Item : in Cached_Page_Info);
   procedure Exclusive_Lock (Item : in Cached_Page_Info);

   procedure Upgrade_To_Exclusive (Item : in Cached_Page_Info);

   procedure Shared_Unlock (Item : in Cached_Page_Info);
   procedure Unlock (Item : in Cached_Page_Info);

   function Exclusive_Locked (Item : in Cached_Page_Info) return Boolean;
   function Update_Locked    (Item : in Cached_Page_Info) return Boolean;
   function Shared_Locked    (Item : in Cached_Page_Info) return Boolean;

private

   type File_Cache_Record;
   type File_Cache is access File_Cache_Record;

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists (File_And_Page);


   --  Cached_Page_Info_Record: a single page in the cache
   --  Cached       : True if this page is in the cache (?)
   --  Dirty        : set to True whenever an exclusive lock is taken
   --  Location     : address of the cached page in the Marlowe file
   --  Last_Access  : when the page was last accessed
   --  References   : how many times the page is referenced
   --  From_Cache   : which cache the page is stored
   --  Page_Lock    : page-specific lock

   type Cached_Page_Info_Record is limited
      record
         Cached      : Boolean            := True;
         Dirty       : Boolean            := False;
         Location    : File_And_Page;
         Last_Access : Ada.Calendar.Time;
         References  : Natural            := 1;
         From_Cache  : File_Cache;
         Page_Lock   : Marlowe.Locks.Lock;
         Info_Lock   : Marlowe.Locks.Lock;
      end record;

   type Cached_Page_Info is access Cached_Page_Info_Record;

end Marlowe.Caches;
