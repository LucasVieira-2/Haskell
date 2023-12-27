--modulo Ficha3 where

data Hora = H Int Int
          deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


horaValida :: Hora -> Bool
horaValida (H h m) = if h >= 0 && h <= 24
                  && m >= 0 && m <= 60 then True else False


comparaHoras :: Hora -> Hora -> Bool
comparaHoras (H h1 m1) (H h2 m2) =
      if h1 > h2 then True
      else if h2 > h1 then False
        else m1 > m2


etapavalida :: Etapa -> Bool
etapavalida (ie,fe) = horaValida ie && horaValida fe && comparaHoras fe ie


viagemVal :: Viagem -> Bool
viagemVal [] = True
viagemVal [e] = etapavalida e
viagemVal (x:y:xs) = 
         etapavalida x
      && etapavalida y
      && comparaHoras (fst y) (snd x)
      && viagemVal (y:xs)


horaparche :: Viagem -> (Hora, Hora)
horaparche v = (fst (head v), snd(last v))


horasparaMinutos :: Hora -> Int
horasparaMinutos (H h m) = 60*h + m 

minutosparaHoras :: Int -> Hora
minutosparaHoras m = H (div m 60) (mod m 60)

diffHoras :: Hora -> Hora -> Int
diffHoras h1 h2 = abs ((horasparaMinutos h1) - (horasparaMinutos h2))

duracaoEtapa :: Etapa -> Int
duracaoEtapa (ie, fe) = diffHoras ie fe

tempototal :: Viagem -> Int 
tempototal [] = 0
tempototal (h:t) = duracaoEtapa h + tempototal t


tempototalespera :: Viagem -> Int
tempototalespera l = 
      let (hp, hc) = horaparche l 
          minTotais = diffHoras hp hc 
      in minTotais - tempototal l


type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]


procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura e ((n,d):xs) = if e == n then Just d else procura e xs 



