nullParser input
  | (input.startsWith litNull) = [null, input.slice litNull.length]
  | otherwise = null
  where litNull = 'null'

boolParser input
  | (input.startsWith litTrue) = [true, input.slice litTrue.length]
  | (input.startsWith litFalse) = [false, input.slice litFalse.length]
  | otherwise = null
  where litTrue = 'true'
        litFalse = 'false'

numberParser input =
  | res == null = null
  | otherwise = [parseFloat res[0], input.slice res[0].length]
  where regex = /^-?(0(?=\D+)|(0(?=\.))|[1-9][0-9]*)(\.?\d*([Ee][-+]?\d+)?)?/g
        res = regex.exec input

stringParser input
  | input[0] != '"' = null
  | otherwise = getString (input.slice 1) 0
  where getString input i = if input[i] == '"'
                                then if input[j].charCodeAt 0 == 92
                                      then getString input (i + 1)
                                      else input.slice 0 i
                                else if input[i] then getString input (i + 1) else null
                                where j = i - 1

print (nullParser 'null, 234.343e+2, "Hello \\"Geek\\"", ')
print (boolParser 'true, 234.343e+2, "Hello \\"Geek\\""')
print (boolParser 'false, 234.343e+2, "Hello \\"Geek\\""')
print (numberParser '123.343e+2, "Hello \\"Geek\\""')
print (stringParser '"Hello \\"Geek\\""')
