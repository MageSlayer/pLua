unit luaParserNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pLuaObject, lua, PasTree;

procedure RegisterExistingNode(L : PLua_State; Name : AnsiString; Node : TPasElement);
procedure PushExistingNode(L : PLua_State; Node : TPasElement);

implementation

uses
  pLua;

function GetFileName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.SourceFilename);
  result := 1;
end;

function GetLineNumber(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  lua_pushinteger(l, itm.SourceLinenumber);
  result := 1;
end;

function GetFullName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.FullName);
  result := 1;
end;

function GetPathName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.PathName);
  result := 1;
end;

function GetElementTypeName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.ElementTypeName);
  result := 1;
end;

function GetName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.Name);
  result := 1;
end;

function GetClassName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  itm := TPasElement(target);
  plua_pushstring(l, itm.ClassName);
  result := 1;
end;

function GetParent(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
  nfo : PLuaInstanceInfo;
begin
  itm := TPasElement(target);
  if itm.Parent <> nil then
    begin
      nfo := plua_GetObjectInfo(l, itm.Parent);
      if assigned(nfo) then
        plua_PushObject(nfo)
      else
        lua_pushnil(l);
    end
  else
    lua_pushnil(l);
  result := 1;
end;

function GetDeclaration(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  itm : TPasElement;
begin
  result := 0;
  itm := TPasElement(target);
  if lua_isboolean(L, paramidxstart) then
    plua_pushstring(l, itm.GetDeclaration(lua_toboolean(l, paramidxstart)))
  else
    plua_pushstring(l, itm.GetDeclaration(true));
  result := 1;
end;

function GetDestType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasPointerType then
    PushExistingNode(l, (target as TPasPointerType).DestType)
  else if target is TPasAliasType then
    PushExistingNode(l, (target as TPasAliasType).DestType)
  else
    result := 0;
end;

function GetPackageName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasModule then
    plua_pushstring(l, (target as TPasModule).PackageName)
  else
    result := 0;
end;

function GetValue(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasResString then
    plua_pushstring(l, (target as TPasResString).Value)
  else if target is TPasEnumValue then
    lua_pushinteger(l, (target as TPasEnumValue).Value)
  else if target is TPasArgument then
    plua_pushstring(l, (target as TPasArgument).Value)
  else if target is TPasVariable then
    plua_pushstring(l, (target as TPasVariable).Value)
  else
    result := 0;
end;

function GetRangeStart(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasRangeType then
    plua_pushstring(l, (target as TPasRangeType).RangeStart)
  else
    result := 0;
end;

function GetRangeEnd(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasRangeType then
    plua_pushstring(l, (target as TPasRangeType).RangeEnd)
  else
    result := 0;
end;

function GetIndexRange(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasArrayType then
    plua_pushstring(l, (target as TPasArrayType).IndexRange)
  else
    result := 0;
end;

function GetIsPacked(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasArrayType then
    lua_pushboolean(l, (target as TPasArrayType).IsPacked)
  else if target is TPasRecordType then
    lua_pushboolean(l, (target as TPasRecordType).IsPacked)
  else if target is TPasClassType then
    lua_pushboolean(l, (target as TPasClassType).IsPacked)
  else
    result := 0;
end;

function GetElType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasArrayType then
    PushExistingNode(l, (target as TPasArrayType).ElType)
  else if target is TPasFileType then
    PushExistingNode(l, (target as TPasFileType).ElType)
  else
    result := 0;
end;

function GetIsValueUsed(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasEnumValue then
    lua_pushboolean(l, (target as TPasEnumValue).IsValueUsed)
  else
    result := 0;
end;

function GetAssignedValue(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasEnumValue then
    plua_pushstring(l, (target as TPasEnumValue).AssignedValue)
  else
    result := 0;
end;

function GetGetEnumNames(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
var
  idx : Integer;
  nl  : TStringList;
  itm : TPasEnumType;
begin
  result := 0;
  if target is TPasEnumType then
    begin
      itm := TPasEnumType(target);
      lua_newtable(L);
      result := 1;
      nl := TStringList.Create;
      try
        itm.GetEnumNames(nl);
        for idx := 0 to nl.Count -1 do
          begin
            lua_pushinteger(l, idx+1);
            plua_pushstring(l, nl[idx]);
            lua_settable(L, -3);
          end;
      finally
        nl.Free;
      end;
    end;
end;

function GetEnumType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasSetType then
    PushExistingNode(l, (target as TPasSetType).EnumType)
  else
    result := 0;
end;

function GetVariantName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasRecordType then
    plua_pushstring(l, (target as TPasRecordType).VariantName)
  else
    result := 0;
end;

function GetVariantType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasRecordType then
    PushExistingNode(l, (target as TPasRecordType).VariantType)
  else
    result := 0;
end;

function GetMembers(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasVariant then
    PushExistingNode(l, (target as TPasVariant).Members)
  else
    result := 0;
end;

function GetObjKind(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasClassType then
    begin
      case (target as TPasClassType).ObjKind of
        okObject : plua_pushstring(l, 'Object');
        okClass : plua_pushstring(l, 'Class');
        okInterface : plua_pushstring(l, 'Interface');
      else
        result := 0;
      end;
    end
  else
    result := 0;
end;

function GetAncestorType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasClassType then
    PushExistingNode(l, (target as TPasClassType).AncestorType)
  else
    result := 0;
end;

function GetInterfaceGUID(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasClassType then
    plua_pushstring(l, (target as TPasClassType).InterfaceGUID)
  else
    result := 0;
end;

function GetAccess(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasArgument then
    begin
      case (target as TPasArgument).Access of
        argDefault : plua_pushstring(l, 'Default');
        argConst : plua_pushstring(l, 'Const');
        argVar : plua_pushstring(l, 'Var');
        argOut : plua_pushstring(l, 'Out');
      else
        result := 0;
      end;
    end
  else
    result := 0;
end;

function GetArgType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasArgument then
    PushExistingNode(l, (target as TPasArgument).ArgType)
  else
    result := 0;
end;

function GetIsOfObject(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasProcedureType then
    lua_pushboolean(l, (target as TPasProcedureType).IsOfObject)
  else
    result := 0;
end;

function GetResultType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasResultElement then
    PushExistingNode(l, (target as TPasResultElement).ResultType)
  else
    result := 0;
end;

function GetResultEl(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasFunctionType then
    PushExistingNode(l, (target as TPasFunctionType).ResultEl)
  else
    result := 0;
end;

function GetRefType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasTypeRef then
    PushExistingNode(l, (target as TPasTypeRef).RefType)
  else
    result := 0;
end;

function GetVarType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasVariable then
    PushExistingNode(l, (target as TPasVariable).VarType)
  else
    result := 0;
end;

function GetModifiers(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasVariable then
    plua_pushstring(l, (target as TPasVariable).Modifiers)
  else
    result := 0;
end;

function GetAbsoluteLocation(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : Integer;
begin
  result := 1;
  if target is TPasVariable then
    plua_pushstring(l, (target as TPasVariable).AbsoluteLocation)
  else
    result := 0;
end;

function GetIndexValue(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    plua_pushstring(l, (target as TPasProperty).IndexValue)
  else
    result := 0;
end;

function GetReadAccessorName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    plua_pushstring(l, (target as TPasProperty).ReadAccessorName)
  else
    result := 0;
end;

function GetWriteAccessorName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    plua_pushstring(l, (target as TPasProperty).WriteAccessorName)
  else
    result := 0;
end;

function GetStoredAccessorName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    plua_pushstring(l, (target as TPasProperty).StoredAccessorName)
  else
    result := 0;
end;

function GetDefaultValue(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    plua_pushstring(l, (target as TPasProperty).DefaultValue)
  else
    result := 0;
end;

function GetIsDefault(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    lua_pushboolean(l, (target as TPasProperty).IsDefault)
  else
    result := 0;
end;

function GetIsNodefault(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProperty then
    lua_pushboolean(l, (target as TPasProperty).IsNodefault)
  else
    result := 0;
end;

function GetTypeName(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedureBase then
    plua_pushstring(l, (target as TPasProcedureBase).TypeName)
  else
    result := 0;
end;

function GetProcType(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    PushExistingNode(l, (target as TPasProcedure).ProcType)
  else
    result := 0;
end;

function GetIsVirtual(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsVirtual)
  else
    result := 0;
end;

function GetIsDynamic(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsDynamic)
  else
    result := 0;
end;

function GetIsAbstract(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsAbstract)
  else
    result := 0;
end;

function GetIsOverride(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsOverride)
  else
    result := 0;
end;

function GetIsOverload(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsOverload)
  else
    result := 0;
end;

function GetIsMessage(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).IsMessage)
  else
    result := 0;
end;

function GetisReintroduced(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).isReintroduced)
  else
    result := 0;
end;

function GetisStatic(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
begin
  result := 1;
  if target is TPasProcedure then
    lua_pushboolean(l, (target as TPasProcedure).isStatic)
  else
    result := 0;
end;

function GetVisibility(target : TObject; l : Plua_State; paramidxstart, paramcount : integer) : integer;
var
  itm : TPasElement;
begin
  result := 1;
  itm := TPasElement(target);
  plua_pushstring(l, VisibilityNames[itm.Visibility]);
end;

procedure InitClassInfo;
var
  ci : PLuaClassInfo;
begin
  ci := ClassTypesList.Add('TPasElement');
  plua_AddClassProperty(ci^, 'Visibility', @GetVisibility, nil);
  plua_AddClassProperty(ci^, 'SourceFilename', @GetFileName, nil);
  plua_AddClassProperty(ci^, 'SourceLinenumber', @GetLineNumber, nil);
  plua_AddClassProperty(ci^, 'FullName', @GetFullName, nil);
  plua_AddClassProperty(ci^, 'PathName', @GetPathName, nil);
  plua_AddClassProperty(ci^, 'ElementTypeName', @GetElementTypeName, nil);
  plua_AddClassProperty(ci^, 'Name', @GetName, nil);
  plua_AddClassProperty(ci^, 'Parent', @GetParent, nil);
  plua_AddClassProperty(ci^, 'ClassName', @GetClassName, nil);
  plua_AddClassProperty(ci^, 'DestType', @GetDestType, nil);
  plua_AddClassProperty(ci^, 'PackageName', @GetPackageName, nil);
  plua_AddClassProperty(ci^, 'Value', @GetValue, nil);
  plua_AddClassProperty(ci^, 'RangeStart', @GetRangeStart, nil);
  plua_AddClassProperty(ci^, 'RangeEnd', @GetRangeEnd, nil);
  plua_AddClassProperty(ci^, 'IndexRange', @GetIndexRange, nil);
  plua_AddClassProperty(ci^, 'IsPacked', @GetIsPacked, nil);
  plua_AddClassProperty(ci^, 'ElType', @GetElType, nil);
  plua_AddClassProperty(ci^, 'IsValueUsed', @GetIsValueUsed, nil);
  plua_AddClassProperty(ci^, 'AssignedValue', @GetAssignedValue, nil);
  plua_AddClassProperty(ci^, 'EnumType', @GetEnumType, nil);
  plua_AddClassProperty(ci^, 'VariantName', @GetVariantName, nil);
  plua_AddClassProperty(ci^, 'VariantType', @GetVariantType, nil);
  plua_AddClassProperty(ci^, 'Members', @GetMembers, nil);
  plua_AddClassProperty(ci^, 'ObjKind', @GetObjKind, nil);
  plua_AddClassProperty(ci^, 'AncestorType', @GetAncestorType, nil);
  plua_AddClassProperty(ci^, 'InterfaceGUID', @GetInterfaceGUID, nil);
  plua_AddClassProperty(ci^, 'Access', @GetAccess, nil);
  plua_AddClassProperty(ci^, 'ArgType', @GetArgType, nil);
  plua_AddClassProperty(ci^, 'IsOfObject', @GetIsOfObject, nil);
  plua_AddClassProperty(ci^, 'ResultType', @GetResultType, nil);
  plua_AddClassProperty(ci^, 'ResultEl', @GetResultEl, nil);
  plua_AddClassProperty(ci^, 'RefType', @GetRefType, nil);
  plua_AddClassProperty(ci^, 'VarType', @GetVarType, nil);
  plua_AddClassProperty(ci^, 'Modifiers', @GetModifiers, nil);
  plua_AddClassProperty(ci^, 'AbsoluteLocation', @GetAbsoluteLocation, nil);
  plua_AddClassProperty(ci^, 'IndexValue', @GetIndexValue, nil);
  plua_AddClassProperty(ci^, 'ReadAccessorName', @GetReadAccessorName, nil);
  plua_AddClassProperty(ci^, 'WriteAccessorName', @GetWriteAccessorName, nil);
  plua_AddClassProperty(ci^, 'StoredAccessorName', @GetStoredAccessorName, nil);
  plua_AddClassProperty(ci^, 'DefaultValue', @GetDefaultValue, nil);
  plua_AddClassProperty(ci^, 'IsDefault', @GetIsDefault, nil);
  plua_AddClassProperty(ci^, 'IsNodefault', @GetIsNodefault, nil);
  plua_AddClassProperty(ci^, 'TypeName', @GetTypeName, nil);
  plua_AddClassProperty(ci^, 'ProcType', @GetProcType, nil);
  plua_AddClassProperty(ci^, 'IsVirtual', @GetIsVirtual, nil);
  plua_AddClassProperty(ci^, 'IsDynamic', @GetIsDynamic, nil);
  plua_AddClassProperty(ci^, 'IsAbstract', @GetIsAbstract, nil);
  plua_AddClassProperty(ci^, 'IsOverride', @GetIsOverride, nil);
  plua_AddClassProperty(ci^, 'IsOverload', @GetIsOverload, nil);
  plua_AddClassProperty(ci^, 'IsMessage', @GetIsMessage, nil);
  plua_AddClassProperty(ci^, 'isReintroduced', @GetisReintroduced, nil);
  plua_AddClassProperty(ci^, 'isStatic', @GetisStatic, nil);

  plua_AddClassMethod(ci^, 'GetDeclaration', @GetDeclaration);
  plua_AddClassMethod(ci^, 'GetGetEnumNames', @GetGetEnumNames);
end;

procedure RegisterExistingNode(L: PLua_State; Name: AnsiString;
  Node: TPasElement);
begin
  plua_registerExisting(l, Name, Node, ClassTypesList['TPasElement']);
end;

procedure PushExistingNode(L: PLua_State; Node: TPasElement);
begin
  plua_pushexisting(l, Node, ClassTypesList['TPasElement']);
end;

initialization
  InitClassInfo;

end.

