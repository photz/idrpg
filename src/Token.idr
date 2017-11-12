module Token

public export

data TokenType = Number
               | Identifier
               | LeftPar
               | RightPar
               | Plus
               | Minus
               | Let
               | Def
               | Mul
               | LeftBrace
               | RightBrace
               | Assign
               | Semi
               | Comma
               | QuestionMark
               | Colon

public export

Eq TokenType where
  Number == Number = True
  Identifier == Identifier = True
  LeftPar == LeftPar = True
  RightPar == RightPar = True
  Plus == Plus = True
  Minus == Minus = True
  Let == Let = True
  Def == Def = True
  Mul == Mul = True
  LeftBrace == LeftBrace = True
  RightBrace == RightBrace = True
  Assign == Assign = True
  Semi == Semi = True
  Comma == Comma = True
  QuestionMark == QuestionMark = True
  Colon == Colon = True
  a == b = False
           
public export

Show TokenType where
    show Number = "Number"
    show Identifier = "Identifier"
    show LeftPar = "LeftPar"
    show RightPar = "RightPar"
    show Plus = "Plus"
    show Minus = "Minus"
    show Let = "Let"
    show Def = "Def"
    show Mul = "Mul"
    show LeftBrace = "LeftBrace"
    show RightBrace = "RightBrace"
    show Assign = "Assign"
    show Semi = "Semi"
    show Comma = "Comma"
    show QuestionMark = "QuestionMark"
    show Colon = "Colon"
