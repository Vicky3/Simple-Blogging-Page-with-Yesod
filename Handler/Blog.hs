module Handler.Blog where

import Import

import           Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html.Renderer.String (renderHtml)

getBlogR :: Int -> Handler Html
getBlogR site = do
                  let postsPerSite = 3

                  when (site < 1) $ redirect $ BlogR 1
                  allPosts <- runDB $ selectList [] [Desc BlogPostDate]
                  let numPosts = length(allPosts)
                  let numPages = if (mod numPosts postsPerSite) > 0
                                   then (quot numPosts postsPerSite) + 1
                                   else quot numPosts postsPerSite
                  when ((site > numPages) && (site /= 1)) $ redirect $ BlogR numPages

                  let previousPage = site-1
                  let nextPage = site+1
                  posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo postsPerSite, OffsetBy (postsPerSite*(site-1))]
                  comments <- runDB $ selectList [] [Desc CommentDate, LimitTo postsPerSite]
                  let firstPost = postsPerSite*(site-1)+1
                  let lastPost = firstPost+length(posts)-1

                  (searchWidget, theEnctype) <- generateFormPost searchForm

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
                    <hr>
                    <form method=post enctype=#{theEnctype}>
                      ^{searchWidget}
                      <button>Search!
                    <hr>
                    $if null posts
                      <h2>Posts
                      No Posts! :(
                    $else
                      <h2>Posts #{firstPost} - #{lastPost}
                      $forall Entity postId (BlogPost title text date) <- posts
                        <article class=post>
                          <header>
                            <h3><a href=@{BlogPostR postId}>#{title}</a>
                          #{text}
                          <footer>
                            posted #{show date}
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
                    <h2>Last Comments
                    $if null comments
                      No Comments! :(
                    $else
                      $forall Entity commentId (Comment bPost author title text date) <- comments
                        <article class=comment>
                          <header>
                            <h3><a href=@{BlogPostR bPost}>#{title}</a>
                          #{text}
                          <footer>
                            commented: #{show date} by #{author}
                          
                    <hr>
                  |]

postBlogR :: Int -> Handler Html
postBlogR site = do
                   ((res,bPostWidget),theEnctype) <- runFormPost searchForm
                   case res of
                     FormSuccess s -> do
                       posts <- runDB $ selectList [] [Desc BlogPostDate]
                       let hitTitle = [ x | x <- posts, (Entity _ (BlogPost y _ _)) <- [x], isInfixOf s y]
                       let hitText = [ x | x <- posts, (Entity _ (BlogPost _ y _)) <- [x], isInfixOf s (pack $ renderHtml y)]
                       defaultLayout $ [whamlet|
                         <h1>Results:
                         <table>
                          <tr>
                            <td>
                              <button>Login
                            <td>
                              <form method=get action=@{BlogR 1}>
                                <button>Home
                            <td>
                              <form method=get action=@{AddPostR}>
                                <button>New Post
                            <td>
                              <form method=get action=@{SettingsR}>
                                <button>Settings
                         <h2>Caution: Search may not be that cool... e.g. when there are html tags between words<br>and yes... it's case sensitive
                         <h1>Hits in Post Titles:
                         $if null hitTitle
                           No results in titles.
                         $else
                           $forall Entity postId (BlogPost title text date) <- hitTitle
                             <article class=post>
                               <header>
                               <h3><a href=@{BlogPostR postId}>#{title}</a>
                               #{text}
                               <footer>
                                 posted #{show date}
                         <h1>Hits in Post Texts:
                         $if null hitText
                           No results in texts.
                         $else
                           $forall Entity postId (BlogPost title text date) <- hitText
                             <article class=post>
                               <header>
                               <h3><a href=@{BlogPostR postId}>#{title}</a>
                               #{text}
                               <footer>
                                 posted #{show date}
                         <hr>
                       |]
                     _ -> defaultLayout $ [whamlet|
                       <h1>Sorry, something went wrong!
                         <form method=get action=@{BlogR 1}>
                           <button>Return to main page
                       <hr>
                     |]

searchForm :: Form T.Text
searchForm = renderDivs $ areq textField "Search a word: " Nothing
