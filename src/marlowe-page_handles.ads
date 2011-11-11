with Ada.Finalization;

with Marlowe.Caches;
with Marlowe.Pages;

package Marlowe.Page_Handles is

   type Page_Handle is new Ada.Finalization.Controlled with private;

   procedure Initialize (Handle : in out Page_Handle);
   procedure Finalize   (Handle : in out Page_Handle);
   procedure Adjust     (Handle : in out Page_Handle);

   procedure Set_Cache (Handle : in out Page_Handle;
                        Cache  : in     Marlowe.Caches.File_Cache);

   procedure Set_Page (Handle    : in out Page_Handle;
                       Reference : in     File_And_Page);
   --  Load a page from disk or cache

   procedure Derive (Handle : in     Page_Handle;
                     Result :    out Page_Handle'Class);
   --  Derive one page from another; i.e. set up the result
   --  page based on the values in Handle

   procedure New_Page (Handle    : in out Page_Handle;
                       Reference : in     File_And_Page);
   --  Creates a new page with the given reference

   function Get_Location (Handle : Page_Handle) return File_And_Page;

   function Get_Page (Handle : Page_Handle) return Marlowe.Pages.Page;

   procedure Set_Dirty (Handle : Page_Handle);
   procedure Set_Accessed (Handle : Page_Handle);

   procedure Shared_Lock    (Item : in Page_Handle);
   procedure Update_Lock    (Item : in Page_Handle);
   procedure Exclusive_Lock (Item : in Page_Handle);

   procedure Upgrade_To_Exclusive (Item : in Page_Handle);

   procedure Shared_Unlock (Item : in Page_Handle);
   procedure Unlock (Item : in Page_Handle);

   function Exclusive_Locked (Item : in Page_Handle) return Boolean;
   function Update_Locked    (Item : in Page_Handle) return Boolean;
   function Shared_Locked    (Item : in Page_Handle) return Boolean;

private

   type Page_Handle is new Ada.Finalization.Controlled with
      record
         Cache     : Marlowe.Caches.File_Cache;
         Info      : Marlowe.Caches.Cached_Page_Info;
         The_Page  : Marlowe.Pages.Page;
      end record;

end Marlowe.Page_Handles;
