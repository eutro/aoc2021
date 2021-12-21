import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

data Player = Player {remaining::Int,pos::Int} deriving (Eq, Ord, Ix, Show)

main :: IO ()
main =
  read . ("("++) . (++")") . intercalate ","
  . map (drop (length "Player N starting position: "))
  . take 2 . lines <$> getContents
  >>= mapM_ print .
  (listArray ((1,1), (10,10))
   [[598416, 32491093007709], [598416, 27674034218179], [897798, 48868319769358], [598416, 97774467368562], [432450, 138508043837521], [604998, 157253621231420], [604998, 141740702114011], [897798, 115864149937553], [598416, 85048040806299], [428736, 57328067654557],
    [797160, 27464148626406], [797160, 24411161361207], [1196172, 45771240990345], [797160, 93049942628388], [576600, 131888061854776], [805932, 149195946847792], [805932, 133029050096658], [1196172, 106768284484217], [797160, 76262326668116], [571032, 49975322685009],
    [995904, 51863007694527], [995904, 45198749672670], [1073709, 93013662727308], [995904, 193753136998081], [720750, 275067741811212], [1006866, 309991007938181], [1006866, 273042027784929], [1067724, 214368059463212], [995904, 147573255754448], [713328, 92399285032143],
    [913560, 110271560863819], [908595, 91559198282731], [734820, 193170338541590], [898665, 404904579900696], [864900, 575111835924670], [888735, 647608359455719], [893700, 568867175661958], [739785, 444356092776315], [903630, 303121579983974], [855624, 187451244607486],
    [989352, 156667189442502], [989352, 129742452789556], [1073709, 274195599086465], [989352, 575025114466224], [720750, 816800855030343], [1002474, 919758187195363], [1002474, 807873766901514], [1067724, 630947104784464], [989352, 430229563871565], [711480, 265845890886828],
    [929625, 175731756652760], [926610, 146854918035875], [752745, 309196008717909], [920580, 647920021341197], [864900, 920342039518611], [914550, 1036584236547450], [921585, 911090395997650], [757770, 712381680443927], [925605, 486638407378784], [853776, 301304993766094],
    [684495, 152587196649184], [678468, 131180774190079], [551901, 272847859601291], [675024, 570239341223618], [798147, 809953813657517], [671580, 912857726749764], [674163, 803934725594806], [556206, 630797200227453], [679329, 433315766324816], [802452, 270005289024391],
    [518418, 116741133558209], [513936, 105619718613031], [412344, 214924284932572], [504972, 446968027750017], [597600, 634769613696613], [503478, 716241959649754], [506466, 632979211251440], [419814, 499714329362294], [512442, 346642902541848], [605070, 218433063958910],
    [998088, 83778196139157], [998088, 75823864479001], [1073709, 148747830493442], [998088, 306621346123766], [720750, 435288918824107], [1004670, 492043106122795], [1004670, 437256456198320], [1067724, 348577682881276], [998088, 245605000281051], [707784, 157595953724471],
    [920079, 56852759190649], [916083, 49982165861983], [742257, 93726416205179], [908091, 190897246590017], [864900, 270803396243039], [900099, 306719685234774], [906093, 274291038026362], [752247, 221109915584112], [918081, 158631174219251], [850080, 104001566545663]]
  !)

main1 = do
  players <- parsePlayers <$> getContents
  print $ uncurry (*) $ mapRight (1000-) $ deterministicDie $ players 1000
  print $ uncurry max $ quantumDie $ players 21
  return ()
  where parsePlayers :: String -> Int -> (Player, Player)
        parsePlayers input = makePlayers
          where makePlayers scoreToWin = listToP $ Player <$> [scoreToWin] <*> starts
                starts = read . drop (length "Player N starting position: ")
                         <$> take 2 (lines input)

        deterministicDie :: (Player, Player) -> (Int, Int)
        deterministicDie start = (totalRolls, loserRem)
          where totalRolls = 3 * length unwonStates
                loserRem = remaining $ snd (last unwonStates)

                unwonStates = takeWhile unwon stateSeq
                  where unwon (_, p2) = remaining p2 > 0

                stateSeq = scanl stepState start tripleRolls
                  where stepState :: FoldFn (Player, Player) Int
                        stepState (p1, p2) roll = (p2, stepPlayer roll p1)

                tripleRolls = map (sum . take 3) $ iterate (drop 3) singleRolls
                  where singleRolls = cycle [1..100]

        quantumDie :: (Player, Player) -> (Integer, Integer)
        quantumDie = compute `memoisedOver` playerDomain
          where compute (p1, p2) = sumPos $ map computeRoll tripleRolls
                  where computeRoll (roll, freq)
                          | remaining p1' <= 0 = (freq, 0)
                          | otherwise = mapBoth (freq*) $ swap $ quantumDie (p2, p1')
                          where p1' = stepPlayer roll p1

                memoisedOver :: Ix a => (a -> b) -> (a, a) -> a -> b
                f `memoisedOver` domain = (!) $ listArray domain $ map f $ range domain

                playerDomain = (join (,) (Player 1 1), join (,) (Player 21 10))

                tripleRolls :: [(Int, Integer)]
                tripleRolls = Map.toList $ frequencies $ add3 <$> die <*> die <*> die
                  where die = [1,2,3]
                        add3 x y z = x + y + z

        setRem :: Int -> Player -> Player
        setRem x (Player _ p) = Player x p

        stepPlayer :: Int -> Player -> Player
        stepPlayer n (Player r p) = Player (r - p') p'
          where p' = mod1 (p + n) 10
