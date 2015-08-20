module Handler.AddTag where

import Import

getAddTagR :: BlogPostId -> Handler Html
getAddTagR blogPostId = do 
                          (tagWidget, theEnctype) <- generateFormPost (tagForm blogPostId)
                          tags <- runDB $ selectList [TagBlogpost ==. blogPostId] []
                          defaultLayout $ [whamlet|
                            <h1>Add a new tag
                            <table>
                                <tr>
                                  <td>
                                    <form method=get action=@{BlogR 1}>
                                      <button>Home
                                  <td>
                                    <form method=get action=@{SettingsR}>
                                      <button>Settings
                            Tags of this Post: #
                            $if null tags
                              No tags yet.
                            $else
                              <ul>
                                $forall Entity _ (Tag _ title) <- tags
                                  <li> #{title}
                            <form method=post enctype=#{theEnctype}>
                              ^{tagWidget}
                              <button>Submit!
                            <hr>
                          |]

postAddTagR :: BlogPostId -> Handler Html
postAddTagR blogPostId = do 
                           ((res,_),_) <- runFormPost (tagForm blogPostId)
                           case res of
                             FormSuccess tag -> do
                               checked <- runDB $ checkUnique tag
                               case checked of
                                 Just _ -> setMessage $ toHtml $ (tagTitle tag) <> " is already a tag of this post!"
                                 Nothing -> do
                                   _ <- runDB $ insert tag
                                   setMessage $ toHtml $ (tagTitle tag) <> " successfully added."
                               redirect $ BlogPostR blogPostId
                             _ -> defaultLayout $ [whamlet|
                             <h1>Sorry, something went wrong!
                             <table>
                                <tr>
                                  <td>
                                    <form method=get><button>Try again
                                  <td>
                                    <form method=get action=@{BlogR 1}><button>Return to main page
                             |]

tagForm :: BlogPostId -> Form Tag
tagForm blogPostId = renderDivs $ Tag
    <$> pure blogPostId
    <*> areq textField "New Tag: " Nothing
