module Handler.Blog where

import Import

getBlogR :: site -> Handler Html
getBlogR site = do
                  posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo 3]
                  defaultLayout $ [whamlet|
                    <h1>Welcome to a FANTASTIC Blog
                    <button>Login    <form method=get action=@{AddPostR}><button>New Post</form>    <form method=get action=@{SettingsR}><button>Settings
                    <h2>Posts
                    $if null posts
                      No Posts! :(
                    $else
                      $forall Entity pid (BlogPost title text date) <- posts
                        <div>
                          <article class=post>
                            <h3>#{title}
                            #{text}
                          <article class=comment>
                            Hier könnte dein Kommentar stehen!
                            
                    <hr>
                  |]
