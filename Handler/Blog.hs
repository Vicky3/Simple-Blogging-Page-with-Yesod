module Handler.Blog where

import Import

getBlogR :: Int -> Handler Html
getBlogR site = do
                  let previousPage = site-1
                  let nextPage = site+1
                  let postsPerSite = 3
                  let firstPost = postsPerSite*(site-1)+1
                  posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo postsPerSite, OffsetBy (postsPerSite*(site-1))]
                  let lastPost = firstPost+length(posts)-1
                  defaultLayout $ [whamlet|
                    <h1>Welcome to a FANTASTIC Blog
                    <button>Login    <form method=get action=@{AddPostR}><button>New Post</form>    <form method=get action=@{SettingsR}><button>Settings</form>
                    $if null posts
                      <h2>Posts
                      No Posts! :(
                    $else
                      <h2>Posts #{firstPost} - #{lastPost}
                      $forall Entity pid (BlogPost title text date) <- posts
                        <div>
                          <article class=post>
                            <h3>#{title}
                            #{text}
                          <article class=comment>
                            Hier könnte dein Kommentar stehen!
                    <hr>
                    <table>
                      <tr>
                        <td>
                          <form method=get action=@{BlogR previousPage}><button>Last</button></form>
                        <td>
                          Page #{site}
                        <td>
                          <form method=get action=@{BlogR nextPage}><button>Next</button></form>
                    <hr>
                  |]
