// Print top row
printf "    "
for i = 1 to 10 do
  printf "% 4d" i
printfn ""
// Print each row preceded by the row number
for j = 1 to 10 do
  printf "% 4d" j
  for i = 1 to 10 do
    printf "% 4d" (i*j)
  printfn ""
