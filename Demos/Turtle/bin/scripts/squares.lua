SetCanvasSize(800, 600)
print('Clearing image')
print('White = '..HexToInt('FFFFFF'))
Clear(HexToInt('FFFFFF'))

t = Turtle:new()
t.Color = 0

function t:mBox(left, top, right, bottom)
  self.PenDown = false
  self:MoveTo(left, top)
  self.PenDown = true
  self:MoveTo(right, top)
  self:MoveTo(right, bottom)
  self:MoveTo(left, bottom)
  self:MoveTo(left, top)
  self.PenDown = false
end

t:mBox(10, 10, 60, 60)
t.Color = HexToInt('FF0000')
t:mBox(20, 20, 50, 50)
t.Color = HexToInt('0000FF')
t:mBox(30, 30, 40, 40)

function t:lBox(left, top, right, bottom)
  self:MoveTo(left, top)
  self:LineTo(right, top)
  self:LineTo(right, bottom)
  self:LineTo(left, bottom)
  self:LineTo(left, top)
end

t.Color = 0

t:lBox(110, 110, 160, 160)
t.Color = HexToInt('FF0000')
t:lBox(120, 120, 150, 150)
t.Color = HexToInt('0000FF')
t:lBox(130, 130, 140, 140)
