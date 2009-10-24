function serialize(o, pre)
  if pre == nil then
    pre = ''
  end
  if type(o)~='table' then
    if type(o) == 'string' then
      return '"' .. o .. '"'
    elseif type(o) ~= 'function' then
      return o
    else
      return 'function () end'
    end
  else
    local res = '{\n'
    for k, v in pairs(o) do
      res = res .. pre .. "  ['" .. tostring(k) .. "']=" .. serialize(v, pre..'  ') .. ',\n'
    end
    res = res .. pre .. '}'
    return res
  end
end

function BuildTree()
  -- reorginize the nodes into a tree format
  local tree = {}
  for k, v in pairs(nodes) do
    if v.Parent == nil then
      table.insert(tree, v)
      v.Children = v.Children or {}
    else
      if v.Parent.Children == nil then
        v.Parent.Children = {}
      end
      table.insert(v.Parent.Children, v)
    end
  end
  return tree
end
tree = BuildTree()
