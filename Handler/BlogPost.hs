module Handler.BlogPost where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR bPostId = do
                         -- get post, comments and tags from DB
                         bPost <- runDB $ get404 bPostId
                         comments <- runDB $ selectList [CommentBlogpost ==. bPostId] [Desc CommentDate]
                         tags <- runDB $ selectList [TagBlogpost ==. bPostId] []
                         author <- runDB $ selectFirst [UserId ==. blogPostAuthor bPost] []

                         -- widget to add comment
                         (commentWidget, theEnctype) <- generateFormPost (commentForm bPostId)

                         -- flags for the menu
                         maid <- maybeAuthId -- if you're logged in
                         let showHome     = True
                         let showNewPost  = True
                         let showSettings = True

                         -- the output
                         defaultLayout $ do
                           [whamlet| <h2>Post No. #{toPathPiece bPostId} |]
                           $(widgetFile "menuBar")
                           $(widgetFile "showPostWithComments")

postBlogPostR :: BlogPostId -> Handler Html
postBlogPostR bPostId = do
                          ((res,_),_) <- runFormPost (commentForm bPostId)
                          case res of
                            FormSuccess comment -> do
                              -- insert comment in DB and return to Post
                              _ <- runDB $ insert comment
                              setMessage $ toHtml $ (commentTitle comment) <> " successfully created"
                              redirect $ BlogPostR bPostId

                            -- cases FormMissing and FormFailure
                            _ -> defaultLayout $(widgetFile "failure")

commentForm :: BlogPostId -> Form Comment
commentForm postId = renderDivs $ Comment
    <$> pure postId
    <*> areq textField "Author: " (Just "FlotteBiene42")
    <*> areq textField "Title: " Nothing
    <*> areq nicHtmlField "Text: " Nothing
    <*> lift (liftIO getCurrentTime)
