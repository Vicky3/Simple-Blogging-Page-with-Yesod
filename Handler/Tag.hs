{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Tag where

import Import

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

import Text.Blaze.Html.Renderer.String (renderHtml)

getTagR :: Text -> Int -> Handler Html
getTagR tagtitle site = do
                  -- ===Settings===
                  let postsPerSite = 3
                  let maxTagCloud = 10
                  -- ==============

                  -- check if site number is valid
                  when (site < 1) $ do
                                      setMessage $ toHtml $ (show site) <> " is no valid site."
                                      redirect $ TagR tagtitle 1

                  -- get number of all posts with this tag
                  allPosts <- runDB
                        $ E.select
                        $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                            E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                            E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                            E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                            return (blogPost)

                  -- calculate how many sites are constructed
                  let numPosts = length(allPosts)
                  let numPages = if (mod numPosts postsPerSite) > 0
                                   then (quot numPosts postsPerSite) + 1
                                   else quot numPosts postsPerSite

                  -- second check if site number is valid
                  when ((site > numPages) && (site /= 1)) $ do
                                                              setMessage $ toHtml $ (show site) <> " is no valid site."
                                                              redirect $ TagR tagtitle numPages




                  -- DB: get posts and tags
                  let offset = fromIntegral $ postsPerSite*(site-1)
                  posts <- runDB
                        $ E.select
                        $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                            E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                            E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                            E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                            E.limit postsPerSite
                            E.offset offset
                            return (blogPost)
                  (tags :: [(E.Value Text, E.Value Int)]) <- runDB
                        $ E.select
                        $ E.from $ \tag -> do
                            E.groupBy $ tag ^. TagTitle
                            let (countRows' :: E.SqlExpr (E.Value Int)) = E.countRows
                            E.orderBy [E.desc countRows']
                            E.limit maxTagCloud
                            return (tag ^. TagTitle, countRows')

                  -- for navigation
                  let previousPage = site-1
                  let nextPage = site+1
                  let firstPost = postsPerSite*(site-1)+1
                  let lastPost = firstPost+length(posts)-1

                  -- for searching
                  (searchWidget, theEnctype) <- generateFormPost searchForm

                  -- flags for the menu
                  maid <- maybeAuthId -- if you're logged in
                  let showHome     = True
                  let showNewPost  = True
                  let showSettings = True
                  
                  -- the output
                  defaultLayout $ do
                    $(widgetFile "tagsAndSearch")
                    [whamlet| <h1>Tag #{tagtitle} |]
                    $(widgetFile "menuBar")
                    $(widgetFile "lastPosts")
                    $(widgetFile "navigation")

postTagR :: Text -> Int -> Handler Html
postTagR tagtitle _ = do
                        ((res,_),_) <- runFormPost searchForm
                        case res of
                          FormSuccess s -> do
                            -- get filtered posts from DB (this is more clever than the other search)
                            hitTitle <- runDB
                             $ E.select
                             $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                                 E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                                 E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                                 E.where_ (blogPost ^. BlogPostTitle `E.like` (E.%) E.++. E.val s E.++. (E.%))
                                 E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                                 return (blogPost)
                            --this is really ugly... But BlogPostText is Html and not Text and I don't now how to handle a LIKE-Query with Html
                            posts <- runDB
                             $ E.select
                             $ E.from $ \(blogPost `E.InnerJoin` tag) -> do
                                 E.on (blogPost ^. BlogPostId E.==. tag ^. TagBlogpost)
                                 E.where_ (tag ^. TagTitle E.==. E.val tagtitle)
                                 E.orderBy [E.desc (blogPost ^. BlogPostDate)]
                                 return (blogPost)
                            let hitText = [ x | x <- posts, (Entity _ (BlogPost _ _ y _)) <- [x], isInfixOf s (pack $ renderHtml y)]

                            -- flags for the menu
                            maid <- maybeAuthId -- if you're logged in
                            let showHome     = True
                            let showNewPost  = True
                            let showSettings = True

                            -- the output
                            defaultLayout $ do
                              [whamlet| <b>Caution: Search may not be that cool... e.g. when there are html tags between words<br>and yes... it's case sensitive |]
                              [whamlet| <h1>Search Results with Tag #{tagtitle} |]
                              $(widgetFile "menuBar")
                              $(widgetFile "searchResults")

                          -- cases FormMissing and FormFailure
                          _ -> defaultLayout $(widgetFile "failure")

searchForm :: Form Text
searchForm = renderDivs $ areq textField "Search a word: " Nothing
