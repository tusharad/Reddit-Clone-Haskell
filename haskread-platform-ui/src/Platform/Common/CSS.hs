{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Platform.Common.CSS
  ( -- * Layout & Page Structure
    pageContainerCSS,
    flexColumnContainerCSS,
    mainContainerCSS,
    threadListSectionCSS,
    threadListMainCSS,
    sectionTitleHomeCSS,
    sectionSubtitleCSS,
    profileInfoCSS,
    sectionHeadingCSS,

    -- * Forms & Inputs
    formGroupCSS,
    labelCSS,
    inputCSS,
    selectCSS,
    textareaCSS,
    charCountLabelCSS,
    disabledLabelCSS,
    inputFieldCSS,

    -- * Buttons
    primaryButtonCSS,
    secondaryButtonCSS,
    buttonCSS,
    blueBgButtonCSS,
    yellowBgButtonCSS,
    buttonBaseCSS,
    smallPrimaryButtonCSS,
    smallGrayButtonCSS,
    successButtonCSS,
    dangerButtonCSS,
    warningButtonCSS,
    buttonPrimaryCSS,
    buttonSecondaryCSS,
    disabledButtonCSS,

    -- * Cards & Containers
    blueBackgroundRoundedCSS,
    cardContainerCSS,
    modalCardCSS,
    p4CSS,
    p2CSS,
    mt2CSS,
    spaceX2CSS,

    -- * Labels & Text
    labelTextCSS,
    largeTextCSS,
    communityNameCSS,
    linkTextCSS,
    commonLinkCSS,
    dropdownItemCSS,
    sortMenuItemCSS,
    selectedSortMenuItemCSS,
    bottomBorderCSS,
    sectionTitleCSS,
    paddedCSS,
    whiteBackgroundShadowCSS,
    charCountSpanCSS,
    searchResultTitleCSS,
    topVotedBtnCSS,
    reallyLongCSS,
    oauthButtonCSS,
    submitButtonCSS,
    formBaseCSS,
    pageBackgroundCSS,

    -- * Auth Pages
    authLayoutFlexCSS,
    authCardContainerCSS,
    authSectionTitleCSS,

    -- * Navigation & Header
    headerCSS,
    containerFlexCSS,
    siteTitleCSS,
    navContainerCSS,
    searchContainerCSS,

    -- * Comments
    commentVotesCSS,

    -- * Live Search
    searchInputCSS,
    searchPopupContainerCSS,
    searchPopupItemCSS,
    searchNoResultsCSS,
    searchResultDescriptionCSS,

    -- * Footer
    footerBgCSS,
    footerTextCSS,
    footerBrandCSS,
    containerCSS,

    -- * Utility
    centeredCSS,
    flexBetweenItemsStartCSS,
    borderBottomCSS,
    fontBoldCSS,

    -- * Images
    imagePreviewCSS,

    -- * OTP / Login / Register
    buttonGroupCSS,

    -- * Social / External
    linkedInLink,
    threadHeaderCSS,
    threadTitleCSS,
    threadTitleLinkCSS,
    threadDescriptionCSS,
    threadMetaCSS,
    threadActionsCSS,
    threadActionButtonCSS,
    commentCountIconCSS,
    threadActionTextCSS,
  )
where

import Data.String.Interpolate (i)
import Web.Hyperbole (Url)
import Web.View.Types (ClassName)

blueBackgroundRoundedCSS :: ClassName
blueBackgroundRoundedCSS =
  [i| bg-blue-800
    shadow-lg rounded-lg mb-6 overflow-hidden |]

linkTextCSS :: ClassName
linkTextCSS = [i| text-blue-300 hover:text-blue-400 transition |]

commonLinkCSS :: ClassName
commonLinkCSS =
  [i| block p-2 text-white hover:bg-gray-700  hover:underline |]

footerTextCSS :: ClassName
footerTextCSS = [i| text-white |]

footerBrandCSS :: ClassName
footerBrandCSS = [i| text-blue-300 hover:text-blue-400 transition |]

containerCSS :: ClassName
containerCSS = [i| container mx-auto text-center |]

footerBgCSS :: ClassName
footerBgCSS = [i| bg-blue-800 mt-auto py-4 |]

dropdownItemCSS :: ClassName
dropdownItemCSS =
  [i| block w-full text-left px-4 py-2
    text-gray-800 dark:text-gray-200
    hover:bg-blue-100 dark:hover:bg-blue-700 transition |]

sortMenuItemCSS :: ClassName
sortMenuItemCSS =
  [i| flex space-x-4 bg-gray-700 rounded-full shadow-lg p-2 |]

selectedSortMenuItemCSS :: ClassName
selectedSortMenuItemCSS =
  [i| absolute z-10 mt-2 bg-white
    dark:bg-gray-800 rounded-md shadow-lg hidden |]

bottomBorderCSS :: ClassName
bottomBorderCSS = [i| border-b p-4 dark:border-gray-700 |]

largeTextCSS :: ClassName
largeTextCSS = [i| text-lg font-bold text-white |]

communityNameCSS :: ClassName
communityNameCSS = [i| border-b last:border-b-0 border-gray-700 text-white |]

linkedInLink :: Url
linkedInLink = "https://www.linkedin.com/in/tushar-adhatrao/"

centeredCSS :: ClassName
centeredCSS = [i| fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center |]

-- CSS utilities
cardContainerCSS :: ClassName
cardContainerCSS =
  [i| shadow-lg bg-blue-800 
    rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300 |]

paddedCSS :: ClassName
paddedCSS = [i| p-6 |]

sectionTitleCSS :: ClassName
sectionTitleCSS =
  [i| text-2xl font-bold mb-4 bg-blue-600 text-white |]

formGroupCSS :: ClassName
formGroupCSS = [i| mb-4 |]

labelCSS :: ClassName
labelCSS = [i| block bg-blue-800 text-white |]

inputCSS :: ClassName
inputCSS = [i| w-full text-white px-3 py-2 border rounded |]

selectCSS :: ClassName
selectCSS = [i| w-full px-2 bg-blue-800 text-white py-2 border rounded |]

buttonCSS :: ClassName
buttonCSS =
  [i| bg-blue-600 hover:bg-opacity-90 transition
    transform hover:scale-105 text-white px-4 py-2 rounded |]

-- CSS utilities
primaryButtonCSS :: ClassName
primaryButtonCSS = "bg-blue-500 p-2 hover:bg-blue-400 transition transform hover:scale-105"

secondaryButtonCSS :: ClassName
secondaryButtonCSS = "bg-yellow-500 hover:bg-yellow-600 transition transform hover:scale-105"

-- CSS Utilities
headerCSS :: ClassName
headerCSS = "bg-blue-800 shadow-lg w-full z-50"

containerFlexCSS :: ClassName
containerFlexCSS = "container mx-auto px-6 py-4 flex flex-col sm:flex-row justify-between items-center"

siteTitleCSS :: ClassName
siteTitleCSS = "text-3xl font-bold text-white mb-4 sm:mb-0"

navContainerCSS :: ClassName
navContainerCSS = "flex items-center w-full sm:w-auto"

searchContainerCSS :: ClassName
searchContainerCSS = "ml-0 sm:ml-4 mt-4 sm:mt-0 flex flex-wrap justify-center space-x-2"

blueBgButtonCSS :: ClassName
blueBgButtonCSS = "bg-blue-600 hover:bg-blue-500 transition transform hover:scale-105"

yellowBgButtonCSS :: ClassName
yellowBgButtonCSS = "bg-yellow-500 hover:bg-yellow-600 transition transform hover:scale-105"

buttonBaseCSS :: ClassName
buttonBaseCSS =
  [i| flex items-center space-x-1 p-2 rounded-full
      hover:bg-gray-100 dark:hover:bg-gray-700 transition transform hover:scale-105 |]

smallPrimaryButtonCSS :: ClassName
smallPrimaryButtonCSS = "bg-blue-600 hover:bg-blue-500"

smallGrayButtonCSS :: ClassName
smallGrayButtonCSS = "bg-gray-700 hover:bg-gray-900 text-white px-2 py-1 rounded-md"

-- Platform/Common/CSS.hs

whiteBackgroundShadowCSS :: ClassName
whiteBackgroundShadowCSS =
  [i| bg-white dark:bg-gray-800 shadow-lg rounded-lg mb-6 overflow-hidden hover:shadow-xl transition-shadow duration-300 |]

p4CSS :: ClassName
p4CSS = "p-4"

flexBetweenItemsStartCSS :: ClassName
flexBetweenItemsStartCSS =
  "flex flex-col sm:flex-row justify-between items-start sm:items-center"

borderBottomCSS :: ClassName
borderBottomCSS = "border-b dark:border-gray-700"

fontBoldCSS :: ClassName
fontBoldCSS = "font-semibold"

mt2CSS :: ClassName
mt2CSS = "mt-2"

spaceX2CSS :: ClassName
spaceX2CSS = "space-x-2"

p2CSS :: ClassName
p2CSS = "p-2"

commentVotesCSS :: ClassName
commentVotesCSS =
  [i|
  flex items-center space-x-1 p-2 rounded-full
  hover:bg-gray-100 dark:hover:bg-gray-700 |]

textareaCSS :: ClassName
textareaCSS = "w-full px-3 py-2 border rounded"

charCountLabelCSS :: ClassName
charCountLabelCSS = "text-right text-sm text-green-600"

charCountSpanCSS :: ClassName
charCountSpanCSS = "text-right text-sm text-green-600"

buttonGroupCSS :: ClassName
buttonGroupCSS = "flex justify-end space-x-2"

disabledButtonCSS :: ClassName
disabledButtonCSS =
  "opacity-50 cursor-not-allowed"

searchInputCSS :: ClassName
searchInputCSS =
  "w-full px-4 py-2 border rounded-full focus:ring-2 focus:ring-blue-600 dark:bg-gray-700 dark:border-gray-600 dark:text-white"

searchPopupContainerCSS :: ClassName -> ClassName
searchPopupContainerCSS shownClass =
  "absolute top-full left-0 right-0 mt-1 bg-white dark:bg-gray-800 border rounded-md shadow-lg " <> shownClass

searchPopupItemCSS :: ClassName
searchPopupItemCSS =
  "p-2 hover:bg-blue-100 dark:hover:bg-blue-700 transition"

searchNoResultsCSS :: ClassName -> ClassName
searchNoResultsCSS shownClass =
  searchPopupItemCSS <> " " <> shownClass

searchResultTitleCSS :: ClassName
searchResultTitleCSS = mempty -- could add styling if needed later

searchResultDescriptionCSS :: ClassName
searchResultDescriptionCSS = "text-gray-500"

disabledLabelCSS :: ClassName
disabledLabelCSS = "block text-gray-700 italic text-sm"

modalCardCSS :: ClassName
modalCardCSS = "bg-gray-700 p-8 rounded-lg shadow-lg max-w-md w-full"

topVotedBtnCSS :: ClassName
topVotedBtnCSS =
  "px-4 py-2 rounded-full bg-blue-600 text-white font-semibold cursor-pointer shadow transition transform hover:scale-105"

reallyLongCSS :: ClassName
reallyLongCSS =
  "px-4 py-2 rounded-full text-gray-800 dark:text-gray-200 hover:bg-blue-100 dark:hover:bg-gray-700 cursor-pointer transition shadow"

pageContainerCSS :: ClassName
pageContainerCSS = "min-h-screen bg-white dark:bg-gray-800"

flexColumnContainerCSS :: ClassName
flexColumnContainerCSS = "flex flex-col min-h-screen"

mainContainerCSS :: ClassName
mainContainerCSS = "container mx-auto mt-16 px-6 flex-grow"

threadListSectionCSS :: ClassName
threadListSectionCSS = "flex flex-col lg:flex-row gap-6"

threadListMainCSS :: ClassName
threadListMainCSS = "w-full lg:w-3/4 px-4"

sectionTitleHomeCSS :: ClassName
sectionTitleHomeCSS = "text-3xl text-center mb-6 text-gray-800 dark:text-gray-200"

labelTextCSS :: ClassName
labelTextCSS = "text-white"

formBaseCSS :: ClassName
formBaseCSS = "flex flex-col space-y-4"

submitButtonCSS :: ClassName
submitButtonCSS = "rounded text-xl mr-2 bg-blue-600 p-2 text-white"

oauthButtonCSS :: ClassName
oauthButtonCSS = "mt-2 w-full rounded bg-yellow-600 p-2 text-white"

authLayoutFlexCSS :: ClassName
authLayoutFlexCSS = "flex flex-wrap lg:flex-nowrap -mx-4"

authCardContainerCSS :: ClassName
authCardContainerCSS = "w-full lg:w-1/2 px-4"

authSectionTitleCSS :: ClassName
authSectionTitleCSS = "text-2xl font-bold mb-4 text-center"

buttonPrimaryCSS :: ClassName
buttonPrimaryCSS =
  "px-4 py-2 bg-blue-600 text-white rounded hover:bg-gray-500"

buttonSecondaryCSS :: ClassName
buttonSecondaryCSS =
  "px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-500"

imagePreviewCSS :: ClassName
imagePreviewCSS = "w-80 h-80"

inputFieldCSS :: ClassName
inputFieldCSS = "w-full px-3 py-2 border rounded"

successButtonCSS :: ClassName
successButtonCSS = "px-4 py-2 bg-green-600 text-white rounded hover:bg-green-500"

dangerButtonCSS :: ClassName
dangerButtonCSS = "px-4 py-2 bg-red-600 text-white rounded hover:bg-red-500"

warningButtonCSS :: ClassName
warningButtonCSS = "px-4 py-2 bg-yellow-600 text-white rounded hover:bg-yellow-500"

sectionSubtitleCSS :: ClassName
sectionSubtitleCSS = "text-lg text-center mb-6 text-gray-800 dark:text-gray-200"

profileInfoCSS :: ClassName
profileInfoCSS = "text-bold"

sectionHeadingCSS :: ClassName
sectionHeadingCSS = "text-3xl font-bold text-center mb-4"

pageBackgroundCSS :: ClassName
pageBackgroundCSS = "bg-[#F4EEFF]"

threadHeaderCSS :: ClassName
threadHeaderCSS =
  "flex flex-col sm:flex-row justify-between items-start sm:items-center p-4"

threadTitleCSS :: ClassName
threadTitleCSS = "text-lg font-bold text-white"

threadTitleLinkCSS :: ClassName
threadTitleLinkCSS =
  "truncate block hover:text-blue-600 dark:hover:text-blue-400 transition"

threadMetaCSS :: ClassName
threadMetaCSS = "text-sm text-white mt-2 sm:mt-0"

threadDescriptionCSS :: ClassName
threadDescriptionCSS = "p-4 text-white"

threadActionsCSS :: ClassName
threadActionsCSS =
  "flex justify-between items-center p-4"

threadActionButtonCSS :: ClassName
threadActionButtonCSS =
  "flex items-center space-x-1 p-2 text-white rounded-full hover:bg-blue-300 transition transform hover:scale-105"

threadActionTextCSS :: ClassName
threadActionTextCSS = "text-white"

commentCountIconCSS :: ClassName
commentCountIconCSS = "bx bx-comment"
