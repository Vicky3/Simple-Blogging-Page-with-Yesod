module Handler.BlogPostEdit where

import Import

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getBlogPostEditR :: BlogPostId -> Handler Html
getBlogPostEditR blogPostId = do
                                -- get post from DB
                                bPost <- runDB $ get404 blogPostId

                                -- widget to change post
                                (postWidget, theEnctype) <- generateFormPost (postForm bPost)

                                -- flags for the menu
                                maid <- maybeAuthId -- if you're logged in
                                let showHome     = True
                                let showNewPost  = True
                                let showSettings = True

                                -- the output
                                defaultLayout $ do
                                  [whamlet| <h1>Edit Post |]
                                  $(widgetFile "menuBar")
                                  [whamlet|
                                    <form method=post enctype=#{theEnctype}>
                                      ^{postWidget}
                                      <button>Submit!
                                  |]

postBlogPostEditR :: BlogPostId -> Handler Html
postBlogPostEditR blogPostId = do
                                 -- get post to know ID of post to change
                                 bPost <- runDB $ get404 blogPostId
                                 ((res,_),_) <- runFormPost (postForm bPost)
                                 case res of
                                   FormSuccess changedPost -> do
                                     -- replace old data by new and return to main page
                                     _ <- runDB $ replace blogPostId changedPost
                                     setMessage $ toHtml $ (blogPostTitle changedPost) <> " successfully updated."
                                     redirect $ BlogPostR blogPostId

                                   -- cases FormMissing and FormFailure
                                   _ -> defaultLayout $(widgetFile "failure")

postForm :: BlogPost -> Form BlogPost
postForm bPost = renderDivs $ BlogPost
    <$> pure (blogPostAuthor bPost)
    <*> areq textField "Title: " (Just (blogPostTitle bPost))
    <*> areq nicHtmlField "Text: " (Just (blogPostText bPost))
    <*> lift (liftIO getCurrentTime)
