module Handler.AddAdmin where

import Import

getAddAdminR :: Handler Html
getAddAdminR = defaultLayout $ [whamlet|
                 <h1>Dev-Site
                 Add the test admin:
                 <form method=post>
                   <button>ADD!
               |]

postAddAdminR :: Handler Html
postAddAdminR = do
                  let admin = User "adreyer@techfak.uni-bielefeld.de" "TheBoss" True
                  _ <- runDB $ insertBy admin
                  defaultLayout $ [whamlet|
                    <h1>Dev-Site
                    Added user #{userName admin}
                  |]
