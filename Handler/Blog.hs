module Handler.Blog where

import Import

getBlogR :: Int -> Handler Html
getBlogR site = do
                  let postsPerSite = 3

                  when (site < 1) $ redirect $ BlogR 1

                  allPosts <- runDB $ selectList [] [Desc BlogPostDate]
                  let numPosts = length(allPosts)
                  let numPages = if (mod numPosts postsPerSite) > 0
                                   then (quot numPosts postsPerSite) + 1
                                   else quot numPosts postsPerSite
                  when (site > numPages) $ redirect $ BlogR numPages

                  let previousPage = site-1
                  let nextPage = site+1
                  
                  let firstPost = postsPerSite*(site-1)+1
                  posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo postsPerSite, OffsetBy (postsPerSite*(site-1))]
                  let lastPost = firstPost+length(posts)-1
                  defaultLayout $ [whamlet|
                    
                    <h1>Welcome to a FANTASTIC Blog
                    <table>
                      <tr>
                        <td>
                          <button>Login
                        <td>
                          <form method=get action=@{AddPostR}>
                            <button>New Post
                        <td>
                          <form method=get action=@{SettingsR}>
                            <button>Settings
                    $if null posts
                      <h2>Posts
                      No Posts! :(
                    $else
                      <h2>Posts #{firstPost} - #{lastPost}
                      $forall Entity postId (BlogPost title text date) <- posts
                        <div>
                          <article class=post>
                            <header>
                              <h3><a href=@{BlogPostR postId}>#{title}</a>
                            #{text}
                            <footer>
                              posted #{show date}
                          <article class=comment>
                            Hier könnte dein Kommentar stehen!
                    <hr>
                    <table>
                      <tr>
                        $if (site /= 1)
                          <td>
                            <form method=get action=@{BlogR 1}>
                              <button>First
                          <td>
                            <form method=get action=@{BlogR previousPage}>
                              <button>Previous
                        <td>
                          Page #{site} of #{numPages}
                        $if (site /= numPages)
                          <td>
                            <form method=get action=@{BlogR nextPage}>
                              <button>Next
                          <td>
                            <form method=get action=@{BlogR numPages}>
                              <button>Last
                    <hr>
                  |]
