messyMain :: IO()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the Auther?"
    auther <- getLine
    print ("Dear " ++ recipient ++ ",\n" ++ "Thanks for buying " ++ title ++ "\nthanks,\n" ++ auther)


toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart auther = "Thanks,\n" ++ auther

createEmail recipient bookTitle auther = toPart recipient ++ bodyPart bookTitle ++ fromPart auther


main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  bookTitle <- getLine
  print "Who is the Auther?"
  auther <- getLine
  print(createEmail recipient bookTitle auther)