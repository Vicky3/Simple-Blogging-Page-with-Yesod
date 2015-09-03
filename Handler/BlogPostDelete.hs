module Handler.BlogPostDelete where

import Import

getBlogPostDeleteR :: BlogPostId -> Handler Html
getBlogPostDeleteR blogPostId = do
                                  -- get post from DB
                                  bPost <- runDB $ get404 blogPostId
                                  -- ask for confirmation (short enough - not necessary to put in template file)
                                  defaultLayout $ [whamlet|
                                    <h1>Delete Post
                                    Do you really want to delete <i>#{blogPostTitle bPost}</i> with all its comments?
                                    <table>
                                      <tr>
                                        <td>
                                          <form method=post action=@{BlogPostDeleteR blogPostId}>
                                            <button>Yes
                                        <td>
                                          <form method=get action=@{BlogPostR blogPostId}>
                                            <button>No
                                  |]

postBlogPostDeleteR :: BlogPostId -> Handler Html
postBlogPostDeleteR blogPostId = do 
                                   -- delete every comment of this post
                                   runDB $ deleteWhere [CommentBlogpost ==. blogPostId]
                                   -- delete post
                                   runDB $ delete blogPostId
                                   -- return to main page
                                   setMessage "Successfully deleted"
                                   redirect $ BlogR 1
