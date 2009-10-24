Handlers = {}

function Handlers:Reset()
  self.inClass = false
  self.inRecord = false
  self.ClassNode = nil
  self.RecordNode = nil
  self.UsesList = ', '..FileBase
  self.ClassList = {}
  self.InterfaceSection = ''
  self.ImplementationSection = ''
end

function Handlers:AddClass(ClassNode)
  table.insert(self.ClassList, ClassNode)
end

function Handlers:Add(AName, AMethod)
  self[AName] = AMethod
end
flogHandlersIndent = ''

function DefaultHandler(ANode, Children)
  if (ANode == nil) or (Children == nil ) then
    return
  end
  for _, v in pairs(Children) do
    Handlers:Handle(v)
  end
end

function Handlers:Handle(ANode)
  if self[ANode.ClassName] ~= nil then
    local oldIndent = flogHandlersIndent
    flog:write(flogHandlersIndent..'Processing Node '..ANode.Name..'('..ANode.ClassName..')\n')
    flogHandlersIndent = flogHandlersIndent .. '  '
    self[ANode.ClassName](ANode, ANode.Children)
    flogHandlersIndent = oldIndent
    flog:write(flogHandlersIndent..'Completed Node '..ANode.Name..'('..ANode.ClassName..')\n')
  --[[else
    flog:write(flogHandlersIndent..'Unknown Node Type '..ANode.Name..'('..ANode.ClassName..')\n')--]]
  else
    local oldIndent = flogHandlersIndent
    flog:write(flogHandlersIndent..'Processing Node '..ANode.Name..'('..ANode.ClassName..') with DefaultHandler\n')
    flogHandlersIndent = flogHandlersIndent .. '  '
    DefaultHandler(ANode, ANode.Children)
    flogHandlersIndent = oldIndent
    flog:write(flogHandlersIndent..'Completed Node '..ANode.Name..'('..ANode.ClassName..') with DefaultHandler\n')
  end
end

function Handlers:ValidNode(ANode)
  return (type(ANode) == 'table') and (ANode.ClassName ~= nil)
end

function Handlers:Output()
end

require('VariableHandler')
