module Handler.AddPost where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getAddPostR :: Handler Html
getAddPostR = do
                -- find author of new post
                userId <- maybeAuthId
                case userId of
                  -- this should never happen because site can only be accessed when logged in
                  Nothing -> defaultLayout $ [whamlet|
                               <h1>Error
                               Could not find author!
                             |]

                  -- found author
                  Just uId -> do

                    -- widget to get new post
                    (bPostWidget, theEnctype) <- generateFormPost (bPostForm uId)

                    -- flags for the menu
                    maid <- maybeAuthId -- if you're logged in
                    let showHome     = True
                    let showNewPost  = False
                    let showSettings = True

                    -- the output
                    defaultLayout $ do
                      [whamlet| <h1>Add a new post |]
                      $(widgetFile "menuBar")
                      $(widgetFile "newPost")

postAddPostR :: Handler Html
postAddPostR = do
                 -- find author of new post
                 userId <- maybeAuthId
                 case userId of
                   -- this should never happen because site can only be accessed when logged in
                   Nothing -> defaultLayout $ [whamlet|
                                <h1>Error
                                Could not find author!
                              |]

                   -- found author
                   Just uId -> do
                     ((res,_),_) <- runFormPost (bPostForm uId)
                     case res of
                       FormSuccess bPost -> do
                         -- add new post to database
                         bPostId <- runDB $ insert bPost
                         setMessage $ toHtml $ (blogPostTitle bPost) <> " successfully created"
                         -- redirect to main page
                         redirect $ BlogPostR bPostId

                       -- cases FormMissing and FormFailure
                       _ -> defaultLayout $(widgetFile "failure")

bPostForm :: UserId -> Form BlogPost
bPostForm authorId = renderDivs $ BlogPost
    <$> pure authorId
    <*> areq textField "Title: " Nothing
    <*> areq nicHtmlField "Text: " Nothing
    <*> lift (liftIO getCurrentTime)
