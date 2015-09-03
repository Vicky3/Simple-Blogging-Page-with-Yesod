module Handler.AddTag where

import Import

getAddTagR :: BlogPostId -> Handler Html
getAddTagR blogPostId = do 
                          -- get tags already associated with post
                          tags <- runDB $ selectList [TagBlogpost ==. blogPostId] []

                          -- widget to get new tag
                          (tagWidget, theEnctype) <- generateFormPost (tagForm blogPostId)

                          -- flags for the menu
                          maid <- maybeAuthId -- if you're logged in
                          let showHome     = True
                          let showNewPost  = False
                          let showSettings = True

                          -- the output
                          defaultLayout $ do
                            [whamlet| <h1>Add a new tag |]
                            $(widgetFile "menuBar")
                            $(widgetFile "newTag")

postAddTagR :: BlogPostId -> Handler Html
postAddTagR blogPostId = do 
                           ((res,_),_) <- runFormPost (tagForm blogPostId)
                           case res of
                             FormSuccess tag -> do
                               -- check if tag is already associated with this post (and add if not)
                               checked <- runDB $ checkUnique tag
                               case checked of
                                 Just _ -> setMessage $ toHtml $ (tagTitle tag) <> " is already a tag of this post!"
                                 Nothing -> do
                                   _ <- runDB $ insert tag
                                   setMessage $ toHtml $ (tagTitle tag) <> " successfully added."
                               -- redirect to main page (with message chosen above)
                               redirect $ BlogPostR blogPostId

                             -- cases FormMissing and FormFailure
                             _ -> defaultLayout $(widgetFile "failure")

tagForm :: BlogPostId -> Form Tag
tagForm blogPostId = renderDivs $ Tag
    <$> pure blogPostId
    <*> areq textField "New Tag: " Nothing
