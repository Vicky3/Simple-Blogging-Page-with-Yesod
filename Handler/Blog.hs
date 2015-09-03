{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Blog where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto ((^.))

import Text.Blaze.Html.Renderer.String (renderHtml)

getBlogNoSiteR :: Handler Html
getBlogNoSiteR = redirect $ BlogR 1

getBlogR :: Int -> Handler Html
getBlogR site = do
                  -- ===Settings===
                  let postsPerSite = 3
                  let maxTagCloud = 10
                  -- ==============

                  -- check if site number is valid
                  when (site < 1) $ do
                                      setMessage $ toHtml $ (show site) <> " is no valid site."
                                      redirect $ BlogR 1

                  -- get number of all posts and calculate how many sites are constructed
                  allPosts <- runDB $ selectList [] [Desc BlogPostDate]
                  let numPosts = length(allPosts)
                  let numPages = if (mod numPosts postsPerSite) > 0
                                   then (quot numPosts postsPerSite) + 1
                                   else quot numPosts postsPerSite

                  -- second check if site number is valid
                  when ((site > numPages) && (site /= 1)) $ do
                                                              setMessage $ toHtml $ (show site) <> " is no valid site."
                                                              redirect $ BlogR numPages

                  -- Blog name
                  name <- runDB $ selectFirst [] [Asc BlogNameId]
                  let blogName = case name of
                                   Nothing -> "a FANTASTIC blog (blog name not yet chosen)"
                                   Just (Entity _ (BlogName n)) -> n

                  -- DB: get posts, comments and tags
                  posts <- runDB $ selectList [] [Desc BlogPostDate, LimitTo postsPerSite, OffsetBy (postsPerSite*(site-1))]
                  comments <- runDB $ selectList [] [Desc CommentDate, LimitTo postsPerSite]
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
                  let showHome     = False
                  let showNewPost  = True
                  let showSettings = True

                  -- the output
                  defaultLayout $ do
                    $(widgetFile "tagsAndSearch")
                    [whamlet| <h1>Welcome to #{blogName} |]
                    $(widgetFile "menuBar")
                    $(widgetFile "lastPosts")
                    $(widgetFile "navigation")
                    $(widgetFile "lastComments")

postBlogR :: Int -> Handler Html
postBlogR _ = do
                ((res,_),_) <- runFormPost searchForm
                case res of
                   FormSuccess s -> do
                     -- get posts from DB and afterwards search for the keyword (this is really not a good search)
                     posts <- runDB $ selectList [] [Desc BlogPostDate]
                     let hitTitle = [ x | x <- posts, (Entity _ (BlogPost _ y _ _)) <- [x], isInfixOf s y]
                     let hitText = [ x | x <- posts, (Entity _ (BlogPost _ _ y _)) <- [x], isInfixOf s (pack $ renderHtml y)]

                     -- flags for the menu
                     maid <- maybeAuthId
                     let showHome     = True
                     let showNewPost  = True
                     let showSettings = True

                     -- the output
                     defaultLayout $ do
                       [whamlet| <b>Caution: Search may not be that cool... e.g. when there are html tags between words<br>and yes... it's case sensitive |]
                       [whamlet| <h1>Results of your search: |]
                       $(widgetFile "menuBar")
                       $(widgetFile "searchResults")

                   -- cases FormMissing and FormFailure (no idea when this will happen)
                   _ -> defaultLayout $(widgetFile "failure")

searchForm :: Form Text
searchForm = renderDivs $ areq textField "Search a word: " Nothing
