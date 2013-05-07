{$MACRO ON}

{$DEFINE extdecl := cdecl}

//include user-defined defines
{$I defines.inc}

{$IFDEF LUAJIT}
  {$IFDEF CPU64}
  //Currently only x64 exception natively by LuaJit itself
  //other platform need try/except on Pascal->Lua boundary
  {$DEFINE LUAJIT_EXCEPTION_SUPPORT}
  {$ENDIF}
{$ENDIF}

