{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Tag where

import Import

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getTagR :: Text -> Int -> Handler Html
getTagR tagtitle site = do
                  let postsPerSite = 3
                  let maxTagCloud = 10

                  when (site < 1) $ do
                                      setMessage $ toHtml $ (show site) <> " is no valid site."
                                      redirect $ TagR tagtitle 1
                  --allPosts <- runDB $ selectList [] [Desc BlogPostDate]
                  allPosts <- runDB
                        $ E.select
                        $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                            E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                            E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                            --E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                            return (blogPost)

                  let numPosts = length(allPosts)
                  let numPages = if (mod numPosts postsPerSite) > 0
                                   then (quot numPosts postsPerSite) + 1
                                   else quot numPosts postsPerSite
                  when ((site > numPages) && (site /= 1)) $ do
                                                              setMessage $ toHtml $ (show site) <> " is no valid site."
                                                              redirect $ TagR tagtitle numPages

                  name <- runDB $ selectFirst [] [Asc BlogNameId]
                  let blogName = case name of
                                   Nothing -> "a FANTASTIC blog (blog name not yet chosen)"
                                   Just (Entity _ (BlogName n)) -> n

                  let previousPage = site-1
                  let nextPage = site+1
                  let offset = fromIntegral $ postsPerSite*(site-1)
                  --posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo postsPerSite, OffsetBy (postsPerSite*(site-1))]
                  posts <- runDB
                        $ E.select
                        $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                            E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                            E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                            E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                            E.limit postsPerSite
                            E.offset offset
                            return (blogPost)
                  comments <- runDB $ selectList [] [Desc CommentDate, LimitTo postsPerSite]
                  let firstPost = postsPerSite*(site-1)+1
                  let lastPost = firstPost+length(posts)-1

                  (tags :: [(E.Value Text, E.Value Int)]) <- runDB
                        $ E.select
                        $ E.from $ \tag -> do
                            E.groupBy $ tag ^. TagTitle
                            let (countRows' :: E.SqlExpr (E.Value Int)) = E.countRows
                            E.orderBy [E.desc countRows']
                            E.limit maxTagCloud
                            return (tag ^. TagTitle, countRows')

                  maid <- maybeAuthId
                  (searchWidget, theEnctype) <- generateFormPost searchForm
                  
                  defaultLayout $ [whamlet|
                    <aside>
                      <h3>Tag Cloud
                      <ul>
                         $forall (E.Value tagTitle, E.Value count) <- tags
                           <li><font size="#{count}"><a href=@{TagR tagTitle 1}> #{tagTitle}: #{count}</a></font>
                      <hr>
                      <form method=post enctype=#{theEnctype}>
                        ^{searchWidget}
                        <button>Search!
                    <h1>Welcome to #{blogName} - Tag #{tagtitle}
                    <table>
                      <tr>
                        <td>
                          $maybe _ <- maid
                            <form method=get action=@{AuthR LogoutR}>
                              <button>Logout
                          $nothing
                            <form method=get action=@{AuthR LoginR}>
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
                    <hr>
                    $if null posts
                      <h2>Posts
                      No Posts! :(
                    $else
                      <h2>Posts #{firstPost} - #{lastPost}
                      $forall Entity postId (BlogPost author title text date) <- posts
                        <article class=post>
                          <header>
                            <h3><a href=@{BlogPostR postId}>#{title}</a>
                          #{text}
                          <footer>
                            $maybe _ <- maid
                              <a href=@{BlogPostEditR postId}><img alt="Edit" src=@{StaticR edit_png}></a>
                              <a href=@{BlogPostDeleteR postId}><img alt="Delete" src=@{StaticR delete_png}></a>
                            posted: #{formatTime defaultTimeLocale "%c" date}
                    <hr>
                    <table>
                      <tr>
                        $if (site /= 1)
                          <td>
                            <form method=get action=@{TagR tagtitle 1}>
                              <button>First
                          <td>
                            <form method=get action=@{TagR tagtitle previousPage}>
                              <button>Previous
                        <td>
                          Page #{site} of #{numPages}
                        $if (site /= numPages)
                          <td>
                            <form method=get action=@{TagR tagtitle nextPage}>
                              <button>Next
                          <td>
                            <form method=get action=@{TagR tagtitle numPages}>
                              <button>Last
                    <hr>
                    <h2>Last Comments
                    $if null comments
                      No Comments! :(
                    $else
                      $forall Entity _ (Comment bPost author title text date) <- comments
                        <article class=comment>
                          <header>
                            <h3><a href=@{BlogPostR bPost}>#{title}</a>
                          #{text}
                          <footer>
                            commented: #{formatTime defaultTimeLocale "%c" date}
                          
                    <hr>
                  |]

searchForm :: Form Text
searchForm = renderDivs $ areq textField "Search a word: " Nothing
