nullParser input
  | (input.startsWith litNull) = [null, let rem = input.slice litNull.length in rem.trim null]
  | otherwise = null
  where litNull = 'null'

boolParser input
  | (input.startsWith litTrue) = [true, let rem = input.slice litTrue.length in rem.trim null ]
  | (input.startsWith litFalse) = [false, let rem = input.slice litFalse.length in rem.trim null]
  | otherwise = null
  where litTrue = 'true'
        litFalse = 'false'

numberParser input =
  | res == null = null
  | otherwise = [parseFloat res[0], let rem = input.slice res[0].length in rem.trim null]
  where regex = /^-?(0(?=\D+)|(0(?=\.))|[1-9][0-9]*)(\.?\d*([Ee][-+]?\d+)?)?/g
        res = regex.exec input

stringParser input
  | input[0] != '"' = null
  | res == null = null
  | otherwise = [res[0], let rem = input.slice res[0].length in rem.trim null]
  where regex = /^"(?:\\"|[^"])*(?<=[^\\])"/g
        res = regex.exec input

spaceParser input
  | res == null = ['', input]
  | otherwise = [res[0],  input.slice res[0].length]
  where regex = /^\s*/g
        res = regex.exec input

commaParser input
  | input[0] == ',' = [',', let rem = input.slice 1 in rem.trim null]
  | otherwise = null

colonParser input
  | input[0] == ':' = [':', let rem = input.slice 1 in rem.trim null]
  | otherwise = null

valueParser input
  | num != null = num
  | str != null = str
  | bool != null = bool
  | pNull != null = pNull
  where num = numberParser input
        str = stringParser input
        bool = boolParser input
        pNull = nullParser input

arrayParser input arr
  | input[0] != '[' = null
  | otherwise = if punct == ']'
                then [arr.concat val, rem.slice 1]
                else if punct == ','
                     then let rem = rem.slice 1 in arrayParser (rem.trim 0) (arr.concat val)
                     else null
  where xx = print '-->' input arr
        parsed = valueParser if input[0] != '[' then input else (input.slice 1)
        punct = if parsed then let x = parsed[1] in x[0] else null
        val = if parsed then parsed[0] else null
        rem = if parsed then parsed[1] else null
        yy = print '  <--' punct val rem

s = '[123, "abc", 3]'
print (arrayParser s [])
