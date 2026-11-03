module Expenses.Email where

import CustomPrelude

-- | An e-mail from one of the user's Gmail accounts, with its body converted to plain text.
data Email = Email
  { account :: Text
  -- ^ The Gmail address.
  , messageId :: Text
  , from :: Text
  , subject :: Text
  , date :: Text
  , body :: Text
  }
  deriving stock (Show, Eq)

-- | Identifies an e-mail across all the Gmail accounts.
emailId :: Email -> Text
emailId email =
  email.account <> "/" <> email.messageId
