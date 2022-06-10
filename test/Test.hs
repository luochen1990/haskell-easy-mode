import EasyMode

inc = (+1)
double = (*2)

main :: IO ()
main = do
    print(1)
    print(1.inc)
    print(1.double.inc)
    print(1.inc.double)

