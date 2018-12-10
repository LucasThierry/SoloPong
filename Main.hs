module Main(main) where

    import Graphics.Gloss
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.Pure.Game

    window :: Display
    window = InWindow "Race" (600, 600) (10, 10)
    
    background :: Color
    background = black

    data RaceGame = Game { ballLoc :: (Float, Float), ballVel :: (Float, Float), player1 :: Float} deriving Show 

    initialState :: RaceGame
    initialState = Game {ballLoc = (9, 30), ballVel = (40, -120), player1 = 40}

    render :: RaceGame -> Picture
    render game =
        pictures [ball, walls, mkPaddle rose (-260) $ player1 game]
        where
            ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
            ballColor = dark red

    wall :: Float -> Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 600 20
    wallColor = greyN 0.5

    sideWall :: Float -> Picture
    sideWall offset = translate offset 0 $ color wallColor $ rectangleSolid 20 600
    walls = pictures [wall 300, wall (-300), sideWall (300), sideWall(-300)]

    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate y x $ color col $ rectangleSolid 86 26
      , translate y x $ color paddleColor $ rectangleSolid 80 20
      ]

    paddleColor = light (light blue)

    moveBall :: Float -> RaceGame -> RaceGame
    moveBall seconds game =
        game { ballLoc = (x', y') }
        where
            (x, y) = ballLoc game
            (vx, vy) = ballVel game
            x' = x + vx * seconds
            y' = y + vy * seconds

    fps :: Int
    fps = 60

    type Radius = Float 
    type Position = (Float, Float)

    wallCollision :: Position -> Radius -> Bool 
    wallCollision (x, y) radius = topCollision || bottomCollision
        where
            topCollision    = y - radius <= -fromIntegral 600 / 2 
            bottomCollision = y + radius >=  fromIntegral 600 / 2   


    sideCollision :: Position -> Radius -> Bool 
    sideCollision (x, y) radius = leftCollision || rightCollision
        where
            leftCollision    = ((x - radius <= -fromIntegral 600 / 2)  && ( y >= 100 || y < -100 ))
            rightCollision = ((x + radius >=  fromIntegral 600 / 2)  && ( y >= 100 || y < -100 ))     

    paddleCollision :: Position -> Radius -> RaceGame  -> Bool
    paddleCollision (x, y) radius game  = 
        p1Paddle
            where
                py1 = player1 game
            
                p1Paddle = ((y - radius < -fromIntegral 250) && ((x < py1+45) && (x > py1 - 45)))

    wallBounce :: RaceGame -> RaceGame
    wallBounce game =
        game { ballVel = (vx' , vy') }
            where
                radius = 10
                (vx, vy) = ballVel game
                vx' = if (sideCollision (ballLoc game) radius)
                    then
                        -vx
                    else
                        vx

                vy' = if ((wallCollision (ballLoc game) radius) || (paddleCollision (ballLoc game) radius game))
                    then
                        -vy
                    else
                        vy

    update :: Float -> RaceGame -> RaceGame
    update seconds = wallBounce . moveBall seconds

    handleKeys :: Event -> RaceGame -> RaceGame
    handleKeys (EventKey (Char 'r') _ _ _) game =
      game { ballLoc = (9, 30), ballVel = (40, -120), player1 = 40}
    handleKeys (EventKey (Char 'd') _ _ _) game = game {player1 = y'}
                        where
                            y = player1 game
                            y' = if (y < 260)
                                then
                                     y + 20
                                else
                                     y
    handleKeys (EventKey (Char 'a') _ _ _) game = game {player1 = y'}
                        where
                            y = player1 game
                            y' = if (y > -260)
                                then
                                     y - 20
                                else
                                     y
    handleKeys _ game = game

    main :: IO ()
    main = play window background fps initialState render handleKeys update 