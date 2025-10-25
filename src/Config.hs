module Config where

import CustomPrelude

import Control.Lens
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Expenses.NonEmptyText qualified as NET
import Types

{- FOURMOLU_DISABLE -}
accountInfos :: [AccountInfo]
accountInfos =
  [
    AccountInfo "Millennium"  "8ec90740-af12-4abc-b12c-3a4cf08af69d" True False
    ,
    AccountInfo "Activo"      "8b683d99-56cf-4cf5-bb05-7fd182877f9a" False False
    -- ,
    -- AccountInfo "Moey"        "229642fa-e480-4822-a482-00af893c2e6e" True False
    -- ,
    -- AccountInfo "Black"       "9e01653c-1c47-450f-89b5-6f90766424df" True True
    -- ,
    -- AccountInfo "CaetanoGo"   "6556104c-e208-42d4-a744-845d0e5e656d" True True
  ]
{- FOURMOLU_ENABLE -}

admins :: [Username]
admins =
  ( [ "diogo.filipe.acastro"
    , "sonia.d.c.barbosa"
    , "test"
    ]
      <&> Username . NET.unsafeFromText
  )

-- NOTE: this is in UTC time.
cronSchedule :: Text
cronSchedule = "0 2 * * *"

cronUser :: Admin
cronUser = Admin $ Username $ NET.unsafeFromText "cron"

allTagGroups :: HashMap TagGroupName [TagName]
allTagGroups =
  HM.fromList $
    [ ("Renda", ["renda"])
    , ("Groceries", ["groceries"])
    , ("Baby", ["baby", "atividades", "toys"])
    ,
      ( "Contas"
      ,
        [ "gym"
        , "vodafone"
        , "luz"
        , "gás"
        , "agua"
        , "banco"
        , "credito carro"
        , "three"
        , "netflix"
        , "ordem dos enfermeiros"
        , "software"
        , "charity"
        , "limpeza da casa"
        , "telemovel"
        , "seguro de trabalho"
        , "seguro do carro"
        , "seguro de saude"
        , "seguro dental"
        , "seguro de trabalho domestico"
        , "seguro de vida"
        , "school"
        ]
      )
    , ("Saude", ["farmacia", "consultas", "analises", "exames", "internamento"])
    , ("Roupa", ["roupa"])
    , ("Carro", ["car wash", "via verde", "gasolina", "estacionamento", "iuc", "car repairs", "inspeção"])
    , ("Gifts", ["gifts"])
    , ("Go out", ["go out"])
    , ("Takeaway", ["takeaway"])
    , ("Casa", ["casa"])
    , ("Ferias", ["ferias"])
    , ("Eletronica", ["eletronica", "jogos"])
    , ("Gata", ["vet"])
    ]
      & each . _2 . each %~ TagName . NET.unsafeFromText

allKnownTags :: Set.Set TagName
allKnownTags = Set.fromList $ concat $ HM.elems allTagGroups

{- |
( Description to match on,
  the category to assign to this transaction
)
-}
categoryPatterns :: [(Text, TagName)]
categoryPatterns =
  [ ("Joana Galhardo", "renda")
  , ("VIAVERDE", "via verde")
  , ("ALTICE PAY, S.A.", "agua")
  , ("Aguas de Gondo", "agua")
  , ("EDP COMERCIAL", "luz")
  , ("LUZBOA", "luz")
  , ("EDP GAS SERVI", "gás")
  , ("DD PT64114306 GAS SU", "gás")
  , ("DD PT79100553 COMPANHIA DE SE 00681505982", "seguro de saude")
  , ("COMPANHIA DE S 00681505982", "seguro de saude")
  , ("DDPT95102302MED02173395164 DI56118299", "seguro de saude")
  , ("DD MEDIS     SAUDE    DI56118299", "seguro de saude")
  , ("DD MEDIS     SAUDE    DI56162829", "seguro dental")
  , ("DD PT18100002 Tranquilidade   82654090063", "seguro do carro")
  , ("82654090063", "seguro do carro")
  , ("GENERALI SEGUROS, SA", "seguro de trabalho")
  , ("PT83100675 LIBERTY SEGUROS 60058549711", "seguro de trabalho domestico") -- seguro de acidentes de trabalho da D. Manuela
  , ("PT83100675 GENERALI SEGURO 60058549711", "seguro de trabalho domestico") -- seguro de acidentes de trabalho da D. Manuela
  , ("60058549711", "seguro de trabalho domestico") -- seguro de acidentes de trabalho da D. Manuela
  , ("DD PT28104381 APRIL PORTUGAL, 00717062587-01637", "seguro de vida")
  , ("DD APRIL PORTUGAL", "seguro de vida")
  , ("ASISA VIDA SEG", "seguro de vida")
  , ("DD PT96113924 AT - AUTORIDADE AT202200000000168", "iuc")
  , ("DD PT85112861 TOYOTA KREDITBA", "credito carro")
  , ("TOYOTA KREDITBANK", "credito carro")
  , ("VODAFONE", "vodafone")
  , ("COMPRA 7765 WOO https", "telemovel")
  , ("COMPRA 7765 WOO ", "telemovel")
  , ("TRF P  Carlos Gomes", "netflix")
  , ("Netflix.com", "netflix")
  , ("NETFLIX.COM", "netflix")
  , ("NETFLIX INTERNATIONAL", "netflix")
  , ("WWW.THREE.CO.UK", "three")
  , ("THREE-TOPUP", "three")
  , ("COM.MAN.CONTA PACOTE CLIENTE FREQUENTE", "banco")
  , ("IMPOSTO SELO ART 17.3.4", "banco")
  , ("A.S.RIO TINTO", "gasolina")
  , ("POSTO GALP", "gasolina")
  , ("AVAUCHER", "gasolina")
  , ("DISA GONDOMAR", "gasolina") -- Bomba Shell em Baguim do Monte
  , ("DDPT40100188 SOLINCA HF", "gym")
  , ("Solinca", "gym")
  , ("SOLINCA", "gym")
  , ("Amnistia Inter", "charity")
  , ("TRF MB WAY P       5181", "cabelo") -- Cabeleireiro Visagisme, Millennium
  , ("Trf MB WAY 910415181", "cabelo") -- Cabeleireiro Visagisme, Moey
  , ("TRF MB WAY P  ALICE PINTO", "cabelo") -- Cabeleireiro Visagisme
  , ("TRF MB WAY P  ISABEL MARIA GONCALVES", "cabelo") -- Cabeleireiro Alter Ego, D. Isabel, onde o David corta o cabelo
  , ("TRF P  D. Manuela", "limpeza da casa")
  , ("TRF P  D Manuela", "limpeza da casa")
  , ("PAG S.SOC 11153829633", "limpeza da casa")
  , -- Baby
    ("PROJETOS DE GENTE", "consultas")
  , ("FISIOTRIMTRIM", "consultas")
  , ("FisioTrimTrim", "consultas")
  , ("FISIOKIDS", "consultas")
  , ("ESTRADA RISONHA", "consultas") -- Clinica Parque da Cidade Junior, Dentista David
  , ("CONTINENTE", "groceries")
  , ("Continente Pay", "groceries")
  , ("BONJOUR Portugal PT", "groceries") -- Continente
  , ("AUCHAN", "groceries")
  , ("PINGO DOCE", "groceries")
  , ("MERCADONA", "groceries")
  , ("TRF MB WAY P       1884", "groceries") -- Sandra Melo
  , ("MYPROTEIN.COM", "groceries")
  , ("A FRESCURA DO POMAR", "groceries") -- frutaria à beira da farmácia silveira
  , ("O GORDO DA FRUTA", "groceries")
  , ("LIDL AGRADEC", "groceries")
  , ("O rapaz da quinta", "groceries")
  , ("M MADALENA M ALVES", "groceries") -- frutaria à beira de nossa casa, na cidade jovem
  , (" ALDI RIO TINTO", "groceries")
  , ("BIODIVERSUS", "groceries") -- Mercadinho Biológico em Gondomar
  , ("CELIA TERESA MARTINS", "groceries") -- Mercadinho Biológico em Gondomar
  , ("Sabores E Utilidades4510 552 Fanzeres", "groceries") -- frutaria à beira de casa dos meus pais
  , ("COMPRA SABORES E UT 9164904/02", "groceries") -- frutaria à beira de casa dos meus pais
  , ("TRF MB WAY P       6180", "groceries") -- cesto de vegetais de A Quintinha
  , ("TRF MB WAY P  MARTA BEATRIZ LOPES MARQUES", "groceries") -- cesto de vegetais de A Quintinha
  , ("TRF MB WAY P  VITOR JOSE SA MARQUES", "groceries") -- cesto de vegetais de A Quintinha
  , ("UBER EATS", "takeaway")
  , ("UBER *EATS", "takeaway")
  , ("UBER   *EATS", "takeaway")
  , ("MADUREIRAS", "takeaway")
  , ("CONFEITARIA IMPERIO", "takeaway")
  , ("COMIDA E VOLTA", "takeaway")
  , ("SUBENSHI", "takeaway")
  , ("BRASINHAS", "takeaway")
  , ("EMPANADAS ARGENTINAS", "takeaway")
  , ("S MARTINO", "takeaway")
  , ("Too Good To Go", "takeaway")
  , ("GONDUREST LDA PORTO", "takeaway") -- mcdonalds
  , ("PIZZAHUT", "takeaway")
  , ("BURGUER KING", "takeaway")
  , ("BK CHAVES BAGUIM DO MON", "takeaway")
  , ("CHURRASQ PRESTIGE", "takeaway") -- Churrasqueira Prestige, ao lado da casa da Cristina em Gondomar
  , ("IRISH CO VILA NOVA DE GAIA", "go out")
  , ("BEIJOBAR MATOSINHOS", "go out")
  , ("LAIS DE GUIA MATOSINHOS", "go out")
  , ("REST MAR A VISTA VILA NOVA GAIA", "go out")
  , ("AQUA REST BAR VILA NOVA GAIA", "go out")
  , ("CEDILHA DOCE", "go out") -- bar do parque de rio tinto
  , ("COMPRA 0288 FARMACIA", "farmacia")
  , ("FARMACIA MAGALHAES FANZERES", "farmacia")
  , ("WELLS BAGUIM", "farmacia")
  , ("WELL S ANTAS PORTO", "farmacia")
  , ("CENTRO VETERINARIO", "vet")
  , -- Atividades do David
    ("GULI FAMILY CLUB", "atividades")
  , ("RITMOS ACROBATA", "atividades") -- Little Gym
  , ("SARA STEINBERG", "atividades") -- Lono Project
  , -- Black
    ("Devolução % Compras Acumuladas", "groceries")
  , ("DD PT66104764 INST GEST FINAN 23021258204", "seg social")
  , ("INST GEST FINAN", "seg social")
  , ("Serokell OU", "ordenado")
  , ("ORDEM DOS ENFE", "ordem dos enfermeiros")
  , ("TRF. P O  FLOATINGLICIOUS - LDA       MADEMOISE", "ordenado")
  , ("Tito Santos", "contabilista")
  , ("DNH GO DADDY EUROPE", "software")
  , ("DNH GODADDY", "software")
  , ("GITKRAKEN", "software")
  , -- Google One / Google Photos
    ("COMPRA 7765 GOOGLE GOOGLE PLAY APP G.CO HELPPAY", "software")
  , ("REEMBOLSOS A RESIDENTES     AT", "iva reembolso")
  ]
    & each . _2 %~ TagName . NET.unsafeFromText

-- | Transactions with this substring will not be treated as expenses.
notExpenses :: [Text]
notExpenses =
  [ "TRF DE DIOGO FILIPE AZEVEDO CASTRO"
  , "TRF P  Diogo Moey" -- Millennium -> Moey
  , "TRANSF SEPA -ENG DIOGO FILIPE" -- Millennium -> Moey
  , "DD PT41100946 CETELEM         75526515837" -- black
  , "DD PT41100946 CETELEM         75498308722" -- caetano go
  , "DD PT24113086 UNIVERSO"
  , "DD UNIVERSO"
  , -- Black
    "Pagamento de Mensalidade"
  , -- Salário da Sónia
    "TRF. P O  FLOATINGLICIOUS - LDA       MADEMOISE"
  , -- Nao vou ignorar, porque posso querer adicionar algo manualmente no campo "Details"
    -- , "DEPOSITO NUMERARIO"

    -- Transferência da conta Millennium da Sónia
    "TRF DE ENF SONIA DANIELA CARNEIRO BARBOSA"
  , -- Levantamento ATM multibanco
    "LEV ATM "
  , "REEMBOLSOS   IRS"
  ]
