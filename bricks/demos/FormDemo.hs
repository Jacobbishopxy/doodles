{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- file: FormDemo.hs
-- author: Jacob Xie
-- date: 2024/02/29 09:49:06 Thursday
-- brief:

module Main where

import Brick
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms
  ( Form (formFocus, formState),
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
  )
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.TH

data Name
  = NameField
  | AgeField
  | BikeField
  | HandedField
  | PasswordField
  | LeftHandField
  | RightHandField
  | AmbiField
  | AddressField
  deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous
  deriving (Eq, Show)

data UserInfo = UserInfo
  { _name :: T.Text,
    _age :: Int,
    _address :: T.Text,
    _ridesBike :: Bool,
    _handed :: Handedness,
    _password :: T.Text
  }
  deriving (Show)

makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e Name
mkForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Name"
            @@= editTextField name NameField (Just 1),
          label "Address"
            @@= B.borderWithLabel (str "Mailing")
            @@= editTextField address AddressField (Just 3),
          label "Age"
            @@= editShowableField age AgeField,
          label "Password"
            @@= editPasswordField password PasswordField,
          label "Dominant hand"
            @@= radioField
              handed
              [ (LeftHanded, LeftHandField, "Left"),
                (RightHanded, RightHandField, "Right"),
                (Ambidextrous, AmbiField, "Both")
              ],
          label ""
            @@= checkboxField ridesBike BikeField "Do you ride a bicycle?"
        ]

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow)
    ]

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body =
      str $
        "- Name is free-form text\n"
          <> "- Age must be an integer (try entering an\n"
          <> "  invalid age!)\n"
          <> "- Handedness selects from a list of options\n"
          <> "- The last option is a checkbox\n"
          <> "- Enter/Esc quit, mouse interacts with fields"

event :: BrickEvent Name e -> EventM Name (Form UserInfo e Name) ()
event ev = do
  f <- gets formFocus
  case ev of
    VtyEvent (V.EvResize {}) -> return ()
    VtyEvent (V.EvKey V.KEsc []) -> halt
    VtyEvent (V.EvKey V.KEnter [])
      | focusGetCurrent f /= Just AddressField -> halt
    _ -> do
      handleFormEvent ev
      st <- gets formState
      modify $ setFieldValid (st ^. age >= 18) AgeField

app :: App (Form UserInfo e Name) e Name
app =
  App
    { appDraw = draw,
      appHandleEvent = event,
      appChooseCursor = focusRingCursor formFocus,
      appStartEvent = return (),
      appAttrMap = const theMap
    }

main :: IO ()
main = do
  let buildVty = do
        v <- mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      initialUserInfo =
        UserInfo
          { _name = "",
            _address = "",
            _age = 0,
            _handed = RightHanded,
            _ridesBike = False,
            _password = ""
          }

      f = setFieldValid False AgeField $ mkForm initialUserInfo

  -- `customMain` 函数提供给用户控制 `vty` 库的初始化，以及操作 `brick` 处理自定义事件；同时，当
  -- 用户需要自定类型时，它也是 `brick` 的入口。
  --
  -- 这里使用了由 `vty-crossplatform` 包所提供的 `mkVty`，用于 Unix 和 Windows 的构建支持。如果
  -- 有倾向性，也可以直接使用 `vty-unix` 或 `vty-windows` 包。
  initialVty <- buildVty
  f' <- customMain initialVty buildVty Nothing app f

  putStrLn "The starting form state was:"
  print initialUserInfo

  putStrLn "The final form state was:"
  print $ formState f'

  if allFieldsValid f'
    then putStrLn "The final form inputs were valid."
    else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
