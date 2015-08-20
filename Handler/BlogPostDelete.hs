module Handler.BlogPostDelete where

import Import

getBlogPostDeleteR :: BlogPostId -> Handler Html
getBlogPostDeleteR blogPostId = do
                                  bPost <- runDB $ get404 blogPostId
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
                                   runDB $ deleteWhere [CommentBlogpost ==. blogPostId]
                                   runDB $ delete blogPostId
                                   setMessage "Successfully deleted"
                                   redirect $ BlogR 1
