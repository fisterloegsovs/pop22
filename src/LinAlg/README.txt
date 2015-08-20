Dette er skitser til 2 forskellige implementationer af en matrix
bibliotek: Et sæt af immutable Lists efter funktionsparadigmet og et
med mutable arrays efter objektparadigmet. Noget af det skal måske
skal bruges i blok 3/4 i forbindelse med linalgdat, men vi vil ikke
bruge det på PoP. Dem vedlægger jeg. Man oversætter f.eks. versionen
efter funktionsprogrammeringsparadigmet således:

Først laves en dll af modulet (interface og implemenation) samt en xml
af documentationen:

  fsharpc --doc:LinAlgFct.xml -a LinAlgFct.{fsi,fs}

Så laves en exe af test-programmet og dll’en:

  fsharpc -r LinAlgFct.dll testLinAlgFct.fsx

Så køres det i mono

  mono testLinAlgFct.exe

Programmerne repræsenterer min leg og læring med fsharp og Torben og
Kenny, som jeg har haft til at kigge på det, har nogle kommentarer,
men jeg tænker, at det kan være jer en hjælp i starten.
