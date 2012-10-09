with Ada.Exceptions;
with Ada.Text_IO;

with Marlowe.Page_Types;

package body Marlowe.Page_Handles is

   ------------
   -- Adjust --
   ------------

   procedure Adjust     (Handle : in out Page_Handle) is
      use type Marlowe.Pages.Page;
   begin
      if Handle.The_Page /= null then
         Marlowe.Caches.Reference (Handle.Info);
      end if;
   end Adjust;

   ------------
   -- Derive --
   ------------

   procedure Derive (Handle : in     Page_Handle;
                     Result :    out Page_Handle'Class)
   is
   begin
      Set_Cache (Result, Handle.Cache);
   end Derive;

   --------------------
   -- Exclusive_Lock --
   --------------------

   procedure Exclusive_Lock (Item : in Page_Handle) is
   begin
      Marlowe.Caches.Exclusive_Lock (Item.Info);
   end Exclusive_Lock;

   ----------------------
   -- Exclusive_Locked --
   ----------------------

   function Exclusive_Locked (Item : in Page_Handle) return Boolean is
   begin
      return Marlowe.Caches.Exclusive_Locked (Item.Info);
   end Exclusive_Locked;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (Handle : in out Page_Handle) is
      use type Marlowe.Pages.Page;
   begin
      if Handle.The_Page /= null then
--           declare
--              Loc : constant File_And_Page := Get_Location (Handle);
--           begin
--              Trace ("begin Finalizing: " & Image (Loc));
            Set_Accessed (Handle);
            Marlowe.Caches.Unreference (Handle.Info);
            Handle.The_Page := null;
--              Trace ("end Finalizing: " & Image (Loc));
--           end;
      end if;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Exception while finalizing page "
                               & Image (Handle.Get_Location)
                               & ": "
                               & Ada.Exceptions.Exception_Name (E));
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           ("Page type: "
            & Marlowe.Page_Types.Page_Type'Image
              (Marlowe.Pages.Get_Page_Type
                 (Handle.The_Page)));

   end Finalize;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Handle : Page_Handle) return File_And_Page is
   begin
      return Marlowe.Pages.Get_File_And_Page (Handle.The_Page);
   end Get_Location;

   --------------
   -- Get_Page --
   --------------

   function Get_Page (Handle : Page_Handle) return Marlowe.Pages.Page is
   begin
      return Handle.The_Page;
   end Get_Page;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Handle : in out Page_Handle) is
      pragma Unreferenced (Handle);
   begin
      null;
   end Initialize;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (Handle    : in out Page_Handle;
                       Reference : in     File_And_Page)
   is
      use type Marlowe.Pages.Page;
   begin
      if Handle.The_Page /= null then
         Set_Accessed (Handle);
         Marlowe.Caches.Unreference (Handle.Info);
      end if;
      Marlowe.Caches.New_Page (Handle.Cache, Reference, Handle.The_Page,
                               Handle.Info);
      Marlowe.Pages.Initialise (Handle.The_Page,
                                Marlowe.Page_Types.No_Page_Type,
                                Reference);
   end New_Page;

   ------------------
   -- Set_Accessed --
   ------------------

   procedure Set_Accessed (Handle : Page_Handle) is
   begin
      Marlowe.Caches.Set_Accessed (Handle.Info);
   end Set_Accessed;

   ---------------
   -- Set_Cache --
   ---------------

   procedure Set_Cache (Handle : in out Page_Handle;
                        Cache  : in     Marlowe.Caches.File_Cache)
   is
   begin
      Handle.Cache := Cache;
   end Set_Cache;

   ---------------
   -- Set_Dirty --
   ---------------

   procedure Set_Dirty (Handle : Page_Handle) is
   begin
      Set_Accessed (Handle);
      Marlowe.Caches.Set_Dirty (Handle.Info);
   end Set_Dirty;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page (Handle    : in out Page_Handle;
                       Reference : in     File_And_Page)
   is
      use type Marlowe.Pages.Page;
   begin
      if Handle.The_Page /= null then
         Marlowe.Caches.Unreference (Handle.Info);
      end if;
      Marlowe.Caches.Get_Page (Handle.Cache, Reference, Handle.The_Page,
                               Handle.Info);
   end Set_Page;

   -----------------
   -- Shared_Lock --
   -----------------

   procedure Shared_Lock (Item : in Page_Handle) is
   begin
      Marlowe.Caches.Shared_Lock (Item.Info);
   end Shared_Lock;

   -------------------
   -- Shared_Locked --
   -------------------

   function Shared_Locked (Item : in Page_Handle) return Boolean is
   begin
      return Marlowe.Caches.Shared_Locked (Item.Info);
   end Shared_Locked;

   -------------------
   -- Shared_Unlock --
   -------------------

   procedure Shared_Unlock (Item : in Page_Handle) is
   begin
      Marlowe.Caches.Shared_Unlock (Item.Info);
   end Shared_Unlock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : in Page_Handle) is
   begin
      Marlowe.Caches.Unlock (Item.Info);
   end Unlock;

   -----------------
   -- Update_Lock --
   -----------------

   procedure Update_Lock (Item : in Page_Handle) is
   begin
      Marlowe.Caches.Update_Lock (Item.Info);
   end Update_Lock;

   -------------------
   -- Update_Locked --
   -------------------

   function Update_Locked (Item : in Page_Handle) return Boolean is
   begin
      return Marlowe.Caches.Update_Locked (Item.Info);
   end Update_Locked;

   --------------------------
   -- Upgrade_To_Exclusive --
   --------------------------

   procedure Upgrade_To_Exclusive (Item : in Page_Handle) is
   begin
      if Marlowe.Caches.Shared_Locked (Item.Info) then
         delay 0.4;
      end if;

      Marlowe.Caches.Upgrade_To_Exclusive (Item.Info);
   end Upgrade_To_Exclusive;

end Marlowe.Page_Handles;
