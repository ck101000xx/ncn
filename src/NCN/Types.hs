module NCN.Types where
type Token = String
type Email = Text
data User = User {
  email :: Email
}