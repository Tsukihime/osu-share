(*
  *  MinHook - Minimalistic API Hook Library
  *  Copyright (C) 2009 Tsuda Kageyu. All rights reserved.
  *
  *  Redistribution and use in source and binary forms, with or without
  *  modification, are permitted provided that the following conditions
  *  are met:
  *
  *  1. Redistributions of source code must retain the above copyright
  *     notice, this list of conditions and the following disclaimer.
  *  2. Redistributions in binary form must reproduce the above copyright
  *     notice, this list of conditions and the following disclaimer in the
  *     documentation and/or other materials provided with the distribution.
  *  3. The name of the author may not be used to endorse or promote products
  *     derived from this software without specific prior written permission.
  *
  *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
  *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
  *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
  *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  *
  Translated by .Vladimir
*)

(* example
  var
    oldMB: function(hWnd: hWnd; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall;

  function NewMB(hWnd: hWnd; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall;
  begin
    oldMB(hWnd, PWideChar('Hoocked!'), lpCaption, uType);
  end;

  begin
    if MH_Initialize() <> MH_OK then
      Exit;
    MH_CreateHook(@MessageBoxW, @NewMB, @@oldMB);
    MessageBoxW(0, 'TestMB', 'TestMB', 0);
    MH_EnableHook(@MessageBoxW);
    MessageBoxW(0, 'TestMB', 'TestMB', 0);
    MH_DisableHook(@MessageBoxW);
    MessageBoxW(0, 'TestMB', 'TestMB', 0);
    MH_Uninitialize();
  end.
*)

unit MinHook;

interface

type
  MH_STATUS = (
    // Unknown error. Should not be returned.
    MH_UNKNOWN = -1,

    // Successful.
    MH_OK = 0,

    // MinHook is already initialized.
    MH_ERROR_ALREADY_INITIALIZED,

    // MinHook is not initialized yet, or already uninitialized.
    MH_ERROR_NOT_INITIALIZED,

    // The hook for the specified target function is already created.
    MH_ERROR_ALREADY_CREATED,

    // The hook for the specified target function is not created yet.
    MH_ERROR_NOT_CREATED,

    // The hook for the specified target function is already enabled.
    MH_ERROR_ENABLED,

    // The hook for the specified target function is not enabled yet, or already disabled.
    MH_ERROR_DISABLED,

    // The specified pointer is invalid. It points the address of non-allocated and/or non-executable region.
    MH_ERROR_NOT_EXECUTABLE,

    // The specified target function cannot be hooked.
    MH_ERROR_UNSUPPORTED_FUNCTION,

    // Failed to allocate memory.
    MH_ERROR_MEMORY_ALLOC,

    // Failed to change the memory protection.
    MH_ERROR_MEMORY_PROTECT);

  // Initialize the MinHook library.
function MH_Initialize(): MH_STATUS; stdcall; external 'MinHook.dll' name 'MH_Initialize';

// Uninitialize the MinHook library.
function MH_Uninitialize(): MH_STATUS; stdcall; external 'MinHook.dll' name 'MH_Uninitialize';

// Creates the Hook for the specified target function, in disabled state.
// Parameters:
// pTarget    [in]  A pointer to the target function, which will be overridden by the detour function.
// pDetour    [in]  A pointer to the detour function, which will override the target function.
// ppOriginal [out] A pointer to the trampoline function, which will be used to call the original target function.
function MH_CreateHook(pTarget, pDetour: pointer; ppOriginal: PPointer): MH_STATUS; stdcall;
  external 'MinHook.dll' name 'MH_CreateHook';

// Enables the already created hook.
// Parameters:
// pTarget [in] A pointer to the target function.
function MH_EnableHook(pTarget: pointer): MH_STATUS; stdcall; external 'MinHook.dll' name 'MH_EnableHook';

// Disables the already created hook.
// Parameters:
// pTarget [in] A pointer to the target function.
function MH_DisableHook(pTarget: pointer): MH_STATUS; stdcall; external 'MinHook.dll' name 'MH_DisableHook';

implementation

end.
