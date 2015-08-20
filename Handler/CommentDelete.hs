module Handler.CommentDelete where

import Import

getCommentDeleteR :: CommentId -> Handler Html
getCommentDeleteR commentId = do
                                comment <- runDB $ get404 commentId
                                defaultLayout $ [whamlet|
                                  <h1>Delete Comment
                                  Do you really want to delete <i>#{commentTitle comment}</i>?
                                  <table>
                                    <tr>
                                      <td>
                                        <form method=post action=@{CommentDeleteR commentId}>
                                          <button>Yes
                                      <td>
                                        <form method=get action=@{BlogR 1}>
                                          <button>No
                                |]

postCommentDeleteR :: CommentId -> Handler Html
postCommentDeleteR commentId = do 
                                 runDB $ delete commentId
                                 setMessage "Successfully deleted"
                                 redirect $ BlogR 1
