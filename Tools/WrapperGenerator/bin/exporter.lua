flog = io.open("log.txt", "w")
flog:write('Processing Source File "'..SourceFile..'"\n\n')
require('utils')
require('handlers')

Handlers:Reset()
for k, v in pairs(tree) do
  if v.ClassName ~= 'TPasUnresolvedTypeRef' then
    Handlers:Handle(v)
  end
end

OutFileName = FilePath..'plua_'..FileBase..'.pas'
flog:write('Generating output file: '..OutFileName)
fout = io.open(OutFileName, 'w')
if fout~=nil then
  fout:write(Handlers:Output())
  fout:close()
else
  flog:write('!!!EXCEPTION CREATING OUTPUT FILE "'..OutFileName..'"!!!')
end

flog:close()