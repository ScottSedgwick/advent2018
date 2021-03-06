module Day02 (answer1, answer2, difference, testLen) where

import Data.List

testLen :: Int -> String -> Bool
testLen n xs = any (\x -> length x == n) $ group $ sort xs

answer1 :: Int
answer1 = twos * threes
  where
    twos = length $ filter (testLen 2) ds
    threes = length $ filter (testLen 3) ds

d2a21 :: [([Char], [Char])]
d2a21 = filter (\(x,y) -> difference x y == 1) $ allpairs ds

answer2 :: String
answer2 = sameParts $ head d2a21

sameParts :: (String, String) -> String
sameParts ([],_) = []
sameParts (_,[]) = []
sameParts ((x:xs),(y:ys)) = if x == y then x : sameParts (xs,ys) else sameParts (xs,ys)

difference :: Eq a => [a] -> [a] -> Int
difference _ [] = 0
difference [] _ = 0
difference (x:xs) (y:ys) = dxy + difference xs ys
  where
    dxy = if x == y then 0 else 1

allpairs :: [a] -> [(a,a)]
allpairs [] = []
allpairs (x:xs) = pairs x xs ++ allpairs xs

pairs :: a -> [a] -> [(a,a)]
pairs _ [] = []
pairs a (x:xs) = (a,x) : pairs a xs

ds :: [String]
ds = 
  [ "tjxmoewpqkyaiqvmndgflunszc"
  , "tjxmobwpqkyaihvrndgfjubszm"
  , "tjxmzewpqkyaihvrydgflrbszc"
  , "tjxmoeypqkyvihvrndgflubsza"
  , "tjcmoewpqkytihvrndgflgbszc"
  , "tjvmoewpqkyanevrndgflubszc"
  , "tjxmoewpqkdiihirndgflubszc"
  , "tjxboewpqkyaihbrnogflubszc"
  , "ojpmoewpqkyaihvjndgflubszc"
  , "tjxyoewpqkyaiuvrndgflutszc"
  , "tjxmoewpqkyalhvrndmflebszc"
  , "tjxmoewpqzyaihhrndgflubszf"
  , "tjxmrewpqkyaihirndgfjubszc"
  , "pjxmoewpqkyaihvendgfbubszc"
  , "txxmkewpqkyjihvrndgflubszc"
  , "tjxmoewcqkyaihvrnmgflubczc"
  , "tjxmoewkqkyaghvrndgfluvszc"
  , "tjxmoewfqkhaihvrndgflubhzc"
  , "jjqmoewpqkyaihvrndzflubszc"
  , "tjxmoewmqksaihvcndgflubszc"
  , "tjrmoewpqkyaihvrvdgflubzzc"
  , "tjxxoewpqkyaiiwrndgflubszc"
  , "cjxmoawxqkyaihvrndgflubszc"
  , "tjxdoewpvkyaihvrndgflubsoc"
  , "tjxmsewpqkyaihvrndgfluzozc"
  , "tjxmoewpqkyafhvrnyeflubszc"
  , "tjxmlewpqkyawhvondgflubszc"
  , "tjxmonwpqkyaiqvrnxgflubszc"
  , "tjxmoewcqkyaihvrnjgflumszc"
  , "tjvmoewpqkyaihveadgflubszc"
  , "tjxmogfpqkyaigvrndgflubszc"
  , "tybmoewpqkyaihvrndgllubszc"
  , "tjxmoewpdkyaihvrndgfluwbzc"
  , "etxmbewpqkyaihvrndgflubszc"
  , "tjxmoeapqcynihvrndgflubszc"
  , "tbxmoewpqkyaihvrndgfdebszc"
  , "haxmoewpqyyaihvrndgflubszc"
  , "ojxmoewpqkyaihvrnegflubszr"
  , "tjxmoewpqkyaihvrndoflubarc"
  , "ljxmoewpqkykihvrndgflvbszc"
  , "tjxmovwpqkyaihvrndgfluzsyc"
  , "tvxmoewpqkyanhvrkdgflubszc"
  , "tjxmoewpqkyaihkrndgfluwwzc"
  , "zjxmoewpfkyaihvrndgfrubszc"
  , "tjxyoegpqkyaihvrndlflubszc"
  , "tjxmoewpqkyamhvrnsgflubmzc"
  , "tjmmoewpqkyaihvrndgftuwszc"
  , "tjxmoewpqbraihvrncgflubszc"
  , "tjxmeeepqkyainvrndgflubszc"
  , "tjemoegpqkyaihvredgflubszc"
  , "tjxmoewpqkyaihvdndgfzubqzc"
  , "tjxmoegrqkyaihfrndgflubszc"
  , "tjxmoewpqxyaihvrndgfluyvzc"
  , "qjxmoewpqkyaiwvrnfgflubszc"
  , "tjxwoewpqkyashkrndgflubszc"
  , "tjzmoewiqkyaihurndgflubszc"
  , "tjumuewpqkyaihvrndgflubssc"
  , "tyxooewpukyaihvrndgflubszc"
  , "tjxvoewpqkyaiivindgflubszc"
  , "ijxmoqwpqkyaihvradgflubszc"
  , "tjxmlewpqkyaihvrhdgflubwzc"
  , "tjxmkewpqkyajhqrndgflubszc"
  , "tjxmoewpqkqaiherndgflurszc"
  , "tjamoewpqkyaizvondgflubszc"
  , "tjxgogwpqkyalhvrndgflubszc"
  , "tjxmoewpqkyachvrndgflubuzq"
  , "tjxmowqpqkyaihvrnegflubszc"
  , "mjxmoewpwkyaihvrndgfkubszc"
  , "tpbmoewpqkyaihvrzdgflubszc"
  , "tjbmoewpqkyaiuvrndgflsbszc"
  , "tjxmoewpqklaghvrndgflubazc"
  , "tjxmoewpqkyrihvrndgwlpbszc"
  , "tjcmoewpqksaiyvrndgflubszc"
  , "tjxmoeapqkymihvindgflubszc"
  , "tjxmdewpqkyafhvrndgflqbszc"
  , "tjxmoewpqxyaihvrndsflubszi"
  , "tjxmoeppqkyaihvrcdgflubszd"
  , "tjxmomwpqkyainvrmdgflubszc"
  , "tjxmovwpqkyaihvrndgfdubdzc"
  , "tjxmoewwqkiaihvrjdgflubszc"
  , "tmxmoewpqkyaifvrndgflubszs"
  , "tbxmoewpqkyaihvrbdgflunszc"
  , "tjxmoewrqkyaihvxndgflubszp"
  , "ujxmoewpqkyaihvxndgflubpzc"
  , "tdxmotwpqkyaihvdndgflubszc"
  , "tjxmvewpqkyaihfrndgtlubszc"
  , "tjfmoewpqkyaihvrnyqflubszc"
  , "tjxfolwzqkyaihvrndgflubszc"
  , "ojrmoiwpqkyaihvrndgflubszc"
  , "tjsmoqwpqkyqihvrndgflubszc"
  , "tjxmohwpqkyaihvrudgflubslc"
  , "tjxtoiwpqkyaihvrnogflubszc"
  , "taxmoewpqkyaiyvrndgfwubszc"
  , "tjxwnezpqkyaihvrndgflubszc"
  , "tjxmyevpqkyaivvrndgflubszc"
  , "tjxdoeopqkyaihvgndgflubszc"
  , "tjxaoewpqkmaihvrndgflufszc"
  , "tjxmoewpqkyaxhvrndgflubncc"
  , "tjxmoewpqkyaihurndgflubbjc"
  , "tjxmjewpqgyaihvrnngflubszc"
  , "tjxmogwpqkyaihvrndgflubbcc"
  , "tjxmoewplkyaihvrnpgflibszc"
  , "tjwmoewpqkyaohvrndgfbubszc"
  , "tjwmoewpqkyaihvrndgfsubszm"
  , "tjxmogwpqkyaihvrndiflubqzc"
  , "tjxmoewpqkyaihvrndgflopshc"
  , "rjxmlewpvkyaihvrndgflubszc"
  , "tjxmogwpakyaihvrndgflzbszc"
  , "tjxmoeppqkyaihvrndgflmxszc"
  , "tjxmoewpqkyhihgrndgfzubszc"
  , "tjxqoewpqkyaihtrndgwlubszc"
  , "tjxnoespqkyaihvrndgflubsuc"
  , "tjmmoewpqkraihvrndgflfbszc"
  , "tjxmoewnqkwaihvrndgflubstc"
  , "tjxmoewpqqyaihvrndgfljbszi"
  , "tjxmoewpqkyaihkrkdgalubszc"
  , "tjxmoewpqkyaihvradgjlurszc"
  , "tvxmoewpqkybihvrndbflubszc"
  , "tjxvoewpqkyaihvradgfoubszc"
  , "tjxmoewpqfyaihvlodgflubszc"
  , "tjxmoewmnkyaiivrndgflubszc"
  , "kjxmoewpqkyaihprndgflcbszc"
  , "hjxmoewpqkcaihvrndgvlubszc"
  , "tjxmoewcqkyaihvrncgfllbszc"
  , "tuxmoewpckyaihvrndoflubszc"
  , "tjxmdewpokyaihvrndgflubszn"
  , "mjxmaewpqkyaqhvrndgflubszc"
  , "tjxmoewpmzyaihvrndgfiubszc"
  , "tjxmoewnqkyvihvrndgflubszk"
  , "tjxmoewpmnyaihvrndgftubszc"
  , "zjxmoewpqkysihvrndgfmubszc"
  , "tjxmoewpqkyaihzrntgflubbzc"
  , "tjxmoewpqkgaihwrndsflubszc"
  , "tjxjoewpqkyaihvrndgflgbizc"
  , "oqxmoewpqkyaihvrndgfldbszc"
  , "wjamoewpqkyaihvfndgflubszc"
  , "tjxmoewtmkyvihvrndgflubszc"
  , "tjlmojwpqkyaihvrndgfludszc"
  , "tjxmowwpqkyaihvrndefludszc"
  , "tjxmoewpqkbaihvrndgfluaszt"
  , "tjxmoewpqkzaahvrodgflubszc"
  , "tjxmoewpqkgaihvrndgflubtpc"
  , "tjxmoenpqkyaihcrndgfqubszc"
  , "tbxmoewpqbyaihvrndgalubszc"
  , "tjvmoewqqkyaihvrndvflubszc"
  , "tjxmoewpqkeaihvundgfaubszc"
  , "txxmoewpqkyaihvrwdgflpbszc"
  , "tzxmoewpqkijihvrndgflubszc"
  , "tjxmoewoqkytiuvrndgflubszc"
  , "tjxmrejplkyaihvrndgflubszc"
  , "tjxmoewpqkynihvrpxgflubszc"
  , "tjxmoewpqkvanhvrndgvlubszc"
  , "tjxmoewpdkyiihvrndgflubszs"
  , "tpxmyewpqkyaihvrndgfeubszc"
  , "tpxmoewpqyyaihvrndhflubszc"
  , "tjsmoewpqkyaihvrndhflubnzc"
  , "tjxmoewpukyaihvrnmgflubwzc"
  , "txxmoewpqlyaihwrndgflubszc"
  , "tjxmoewprkyaiovrndgflubxzc"
  , "tjxmouwpqkyaihzrodgflubszc"
  , "tjxmojwpqkywimvrndgflubszc"
  , "tjxsoewpqkyaihvrzdgqlubszc"
  , "tfxmoewpakyaihvrndgllubszc"
  , "tjhmoewpqiyaihvrndgflubsac"
  , "tjxmoewpqkoaihvrndoflubsxc"
  , "tjxmoewpqkyzpjvrndgflubszc"
  , "tjxmoewpqkyaiharndgzlnbszc"
  , "tjimoevpqkyaihvrndgflubbzc"
  , "tjxsoewpqkyahhvrndgfzubszc"
  , "txxmoewpqkyaimvrrdgflubszc"
  , "tjxmoewpwkyaihvrndpylubszc"
  , "tjxmoewpskyaghvrndgfbubszc"
  , "tjxmuewpqmyaihvrndgfyubszc"
  , "tjxmoewpqkyaihvdndgglubsxc"
  , "xjxmoewpqkyjiovrndgflubszc"
  , "gjxmoewpqkyaihvrndodlubszc"
  , "tjbmoewpqkyaihvridgflvbszc"
  , "tjxmozwpqkyapbvrndgflubszc"
  , "tjxeoewpqkyqihvrndgflubhzc"
  , "tjxdoewpqzyaihvrndgflubsmc"
  , "tjxmwewpqkyathvcndgflubszc"
  , "tjxmoewpszyaihvrndgflusszc"
  , "tuxmoewpqkyaihvrndgfluasxc"
  , "tjemoewpnvyaihvrndgflubszc"
  , "tjxmoewpjkyaihvrndgjlufszc"
  , "tjomoewppkyaihvzndgflubszc"
  , "tjxmvewpqkyaimvrntgflubszc"
  , "rjxmoewpqkyaihvpndgflubszq"
  , "hjxzoewpqkyaihvridgflubszc"
  , "texmoejpqkyaihvrndgflubszx"
  , "tjxcoewpqkyaihbrxdgflubszc"
  , "tjxmoewpnkyaihvrndgfltbsze"
  , "tjxmoewpdkyaihvrndwfluwbzc"
  , "tjxmoewpqryjihkrndgflubszc"
  , "tjlmoewpqkhaihvrndgflubsnc"
  , "tjxmovapqkjaihvrndgflubszc"
  , "tjxvoewpqkyaihqrndgfluyszc"
  , "tjxwoewnqkyaihvrndgfgubszc"
  , "tjdmoewpqklaihvcndgflubszc"
  , "tjxmoewpvkynihvrndgflubskc"
  , "tjxmtewpqkyaihvhndgfluaszc"
  , "tjxmoewpqkyanhvrnpgfluvszc"
  , "tjxmoewpqkyaifvbndgflubspc"
  , "tjxmoexpqknaihvrndgxlubszc"
  , "qjxmoewqqkyaihvrndgflubpzc"
  , "tjxmoewppkyaihvaxdgflubszc"
  , "myxmoewpqkyaihvrudgflubszc"
  , "tjxmwewpmkyaihvrndgflubssc"
  , "tjxmoewpqkyaihvrndgfxqbszq"
  , "tjxmoewhqkyaahvrndgflubbzc"
  , "tbxmoewmqkyaihvrndgflubszu"
  , "toxmolwpqkyaihvrndnflubszc"
  , "tjxmoewhqkyaihvrnrgflubvzc"
  , "yjxmoewcqkyaihvrndgflubfzc"
  , "tjxmoewpqkyamhvrgdgflmbszc"
  , "tjxmtewpqkyaizvrndgfluoszc"
  , "tjxmoewpqzyaihvrntsflubszc"
  , "fjxmoewpqkyaihyrmdgflubszc"
  , "tjxwoewpqcyaihbrndgflubszc"
  , "tjxmoebpqkzaihvrndcflubszc"
  , "tjxmoewpqkyaihvrndnlmubszc"
  , "tjxmoewpqkyaihvrndgeyubskc"
  , "tfxmoewpqryaihvrndgfluiszc"
  , "tjxmoewpqkjaihvynngflubszc"
  , "tjxmoewpqkqaihvonjgflubszc"
  , "tjfmokwpqkyeihvrndgflubszc"
  , "djxmoewpkkyaihvrnmgflubszc"
  , "tjxmiewpqkyaihvrndgflubhlc"
  , "tjxmmejpqkyaihvrnhgflubszc"
  , "djxmoewmqkyaihvrnggflubszc"
  , "tjxmoewpqkyaihvrkggflubsze"
  , "gjxmoewpqkyaihjrndgflvbszc"
  , "tjxmoewpqkyaidvrndgkzubszc"
  , "tjxmoewpqkyaedvrnpgflubszc"
  , "sjxmoewpqkyaihvrnngfluhszc"
  , "tjxmoewpqkuaihvrndghlubxzc"
  , "tjxmoewgqkyuihvrndgftubszc"
  , "tjxmoewpqsyaifvrkdgflubszc"
  , "tjxrrewpqkyaihvrnpgflubszc"
  , "tjxmoezpqkyaihvrwdgflabszc"
  , "tjfmoewpqknaihvrndgflubkzc"
  , "tjxmoewnqkxaihvrndgflubtzc"
  , "tjxmoewpkkyaihvrndgfrnbszc"
  , "tjxmorwpnkqaihvrndgflubszc"
  , "tsxmoewwqkyathvrndgflubszc"
  , "tjxmoeupqkyaihvrndyflubszp"
  , "bjxmoewdqkyaihvrndgflurszc"
  , "tjxmoewpvkyaihvrndoflubszq"
  , "sjxmoewpqkyaihvrndgflubyec"
  , "tjxmoewpqkyaizcrndgfnubszc"
  ]