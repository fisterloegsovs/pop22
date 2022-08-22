
fsharpc --nologo main.fs

mkbundle --simple -o main main.exe








fsharpc --nologo temp.fs








fsharpc --nologo numbers.fs









fsharpc --nologo lines.fs






fsharpc --nologo fibfile.fs





REGULÆRE UDTRYK


open System.Text;;

let r = RegularExpressions.Regex "^[1-9][0-9]*$";;

r.IsMatch "2320";;

r.IsMatch "23d20";;





WEB

fsharpc fetchpage.fs
