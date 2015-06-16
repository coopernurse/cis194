{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.Word (Word8)
import System.Environment (getArgs)

import qualified Data.List as L
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

badtransies :: [Transaction]
badtransies = [Transaction {from = "Shanta Shafer", to = "Nannie Swisher", amount = 23, tid = "e5b3ff48-4e31-408e-ad84-e9b77c594723"},Transaction {from = "Ellyn Furr", to = "Altha Redden", amount = 133, tid = "c06601fe-889b-4838-b5b8-319190bb2130"},Transaction {from = "Caprice Burnett", to = "Lucie Yazzie", amount = 340, tid = "feee1d03-6b20-4727-a69d-4ac78b431f6c"},Transaction {from = "Allen Delagarza", to = "Dorsey Gore", amount = 286, tid = "2477fd27-bff0-4ffb-be3f-617c0d300544"},Transaction {from = "Edris Corbitt", to = "Daria Hawk", amount = 268, tid = "b5081f75-2b4d-426c-952b-0f93f05a74e4"},Transaction {from = "Jennefer Eggleston", to = "Asha Walling", amount = 209, tid = "3004034c-c878-456f-a35c-f5118f3156b0"},Transaction {from = "Alyson Pinkston", to = "Angeline Keck", amount = 241, tid = "bbfc9dff-fb0e-43b7-b18f-44f794874d46"},Transaction {from = "Torie Martino", to = "Shaanan Cohney", amount = 230, tid = "91a9beca-328c-47a2-ba88-952e06d0e2a6"},Transaction {from = "Dia Monson", to = "Donald Sanford", amount = 331, tid = "6fff85ce-0525-46bf-84f5-4895eff9869a"},Transaction {from = "Lucie Yazzie", to = "Genaro Goins", amount = 259, tid = "ea7d8457-c404-45ae-b1d1-f251d7bcf981"},Transaction {from = "Blondell See", to = "Shaanan Cohney", amount = 400, tid = "c7cf7ddc-66ce-4aeb-8e91-4b6d2f34d024"},Transaction {from = "Brenna Woodall", to = "Shaanan Cohney", amount = 224, tid = "5357ac92-1558-444a-9086-928e21ae9565"},Transaction {from = "Ileana Broughton", to = "Kassie Hazel", amount = 85, tid = "67ea8bdc-9365-4732-a28e-0b50d518f83e"},Transaction {from = "Shaunte Foy", to = "Jami Kuntz", amount = 345, tid = "6f1abcd3-72a6-40a4-b969-bfa0f516a986"},Transaction {from = "Jennefer Eggleston", to = "Angeline Keck", amount = 124, tid = "5fd0ed63-08df-47c8-874a-c1d30c5486e8"},Transaction {from = "Altha Redden", to = "Shanta Shafer", amount = 273, tid = "0c49e0a3-c3c2-472a-b08d-59484b2616d4"},Transaction {from = "Reta Salcedo", to = "Aisha Craft", amount = 236, tid = "45d577ab-6a92-417b-8077-117df7c7b63b"},Transaction {from = "Donald Sanford", to = "Shaanan Cohney", amount = 331, tid = "d1a62371-bfe7-4d35-9b9b-e469a474cb4c"},Transaction {from = "Shanta Shafer", to = "Shaanan Cohney", amount = 547, tid = "6017dcdd-0d59-4c41-81e3-7ab17ada3d24"},Transaction {from = "Teodoro Gold", to = "Shaanan Cohney", amount = 190, tid = "0e617909-93d0-4eff-97af-0637890c59c6"},Transaction {from = "Margurite Thorn", to = "Shaanan Cohney", amount = 277, tid = "bfacf440-a33e-430b-94c6-e1afa675c9c4"},Transaction {from = "Elise Carnes", to = "Ileana Broughton", amount = 63, tid = "1c0c9a62-af29-4691-abeb-282390d7c0e4"},Transaction {from = "Johnson Humphrey", to = "Alessandra Blackmon", amount = 263, tid = "a76a1d3f-af14-48fd-9407-0d55855a765e"},Transaction {from = "Johnson Humphrey", to = "Joanie Cramer", amount = 357, tid = "71bbde72-35df-40cd-93c3-ff6c123fe9dc"},Transaction {from = "Joanie Cramer", to = "Shaanan Cohney", amount = 91, tid = "11c0efc8-d052-4679-807c-10a414ee8e02"},Transaction {from = "Lucie Yazzie", to = "Ozella Burkett", amount = 125, tid = "f9e176d4-6589-4755-9f34-8d663c1dc3f6"},Transaction {from = "Nathanial Arce", to = "Jeanetta Hoyt", amount = 321, tid = "896aba87-ade5-494c-9e87-dc1e9230b7ef"},Transaction {from = "Jeraldine Williford", to = "Alyson Pinkston", amount = 257, tid = "65953f2d-0357-4700-89b0-562bfcd0fe5b"},Transaction {from = "Kassie Hazel", to = "Shaanan Cohney", amount = 85, tid = "0a1cbf5c-bf61-4116-b8b4-1c0a3a736bad"},Transaction {from = "Altha Redden", to = "Jeanetta Hoyt", amount = 309, tid = "81b5a2a6-d19f-4aa3-a47c-d73725173a01"},Transaction {from = "Bettye Wooten", to = "Louie Lovelace", amount = 296, tid = "01209091-f78c-4970-906e-90f63c926d79"},Transaction {from = "Leia Klinger", to = "Angeline Keck", amount = 174, tid = "84cb4b80-61a8-46ee-a97a-a826c3b065ea"},Transaction {from = "Roscoe Stamps", to = "Shaanan Cohney", amount = 236, tid = "981487e8-c36e-4e3a-8835-158793dd1787"},Transaction {from = "Teddy Cheng", to = "Shaanan Cohney", amount = 5, tid = "d16a1b85-a4c5-44d7-ac80-11a273c92573"},Transaction {from = "Ericka Beverly", to = "Shaanan Cohney", amount = 190, tid = "a6ba3e62-d1d8-455f-94aa-a9cc391bb1c5"},Transaction {from = "Shaunte Foy", to = "Shaanan Cohney", amount = 615, tid = "d9ae397a-39de-42a1-b536-7bc79ecb44b1"},Transaction {from = "Doreatha Folse", to = "Charlyn Keefer", amount = 58, tid = "142d72c9-2570-449c-8099-5734ed41be97"},Transaction {from = "Altha Redden", to = "Santos Maples", amount = 351, tid = "c892baa7-54c0-4772-bd36-d9173dcb044f"},Transaction {from = "Lissette Epperson", to = "Lucie Yazzie", amount = 103, tid = "418c778a-8a11-458d-a756-d00ccc290d3e"},Transaction {from = "Nathanial Arce", to = "Ignacia Baugh", amount = 340, tid = "a76dffac-2cd8-4344-a5df-1107b944d980"},Transaction {from = "Farah Bateman", to = "Ngan Bratcher", amount = 176, tid = "eb1d94aa-95f6-4713-9ee2-22dc46fcd8f3"},Transaction {from = "Robby Laughlin", to = "Matilda Albright", amount = 58, tid = "ca9fb5cc-db95-4f42-a520-55593c8c4725"},Transaction {from = "Una Nobles", to = "Leia Klinger", amount = 89, tid = "7bc3b57b-70cb-4f24-90e2-12dc71227238"},Transaction {from = "Faustina Wolford", to = "Shaanan Cohney", amount = 648, tid = "617f9df4-01c2-43ce-bec4-e21321a163f9"},Transaction {from = "Gregory Nava", to = "Jennefer Eggleston", amount = 110, tid = "b1e3a399-a070-4671-96dc-f4702aa8cf3c"},Transaction {from = "Asha Walling", to = "Ozella Burkett", amount = 151, tid = "1bd46b06-7783-4e5b-bedf-89ac7a5593b6"},Transaction {from = "Joanie Cramer", to = "Faustina Wolford", amount = 253, tid = "b140a329-0524-4036-9b17-56b6faa3a2e2"},Transaction {from = "Ellyn Furr", to = "Lucie Yazzie", amount = 154, tid = "d2ea8827-d623-4621-8298-33b66fa481d4"},Transaction {from = "Genaro Goins", to = "Una Nobles", amount = 168, tid = "bb94b7c7-75b7-4610-bf85-299b06c8ed5a"},Transaction {from = "Matilda Albright", to = "Shaunte Foy", amount = 119, tid = "97a4a176-c1c9-494f-b12c-c6fd696d480c"},Transaction {from = "Suellen Boatwright", to = "Newton Burks", amount = 137, tid = "ca66cd89-fb49-42f6-97a6-c10a03800593"},Transaction {from = "Caprice Burnett", to = "Shanta Shafer", amount = 121, tid = "bf2b1f37-8254-4385-834a-d1cb35f390c2"},Transaction {from = "Margeret Scully", to = "Felecia Mckeown", amount = 169, tid = "9b31e851-38ac-4140-8bb1-70422888bbfe"},Transaction {from = "Suellen Boatwright", to = "Louie Lovelace", amount = 222, tid = "842497df-0fac-42f9-9eb3-ce7de14d563a"},Transaction {from = "Felipa Sallee", to = "August Jewell", amount = 221, tid = "5a2c93fb-f381-456b-9e4d-d18ffcf75fe4"},Transaction {from = "Dorsey Gore", to = "Cherie Mccool", amount = 379, tid = "2296d1af-7f3d-43ea-b41e-f1d070a897aa"},Transaction {from = "Asha Walling", to = "Shaanan Cohney", amount = 58, tid = "f5ab3a2f-4ae3-406e-8be1-d05a6d1e0176"},Transaction {from = "Una Nobles", to = "Dorsey Gore", amount = 34, tid = "23288493-e2fb-4bef-9306-83667f1efe06"},Transaction {from = "Francie Duggan", to = "Margeret Scully", amount = 148, tid = "42860e63-e035-49d1-a559-8f90deddf748"},Transaction {from = "Colette Pickering", to = "Domenica Gable", amount = 75, tid = "d44a272b-a8f8-4c37-a8df-8eefc77c9841"},Transaction {from = "Genaro Goins", to = "Margurite Thorn", amount = 149, tid = "017dba33-c9d5-4544-a7d9-be4586bdde33"},Transaction {from = "Elana Holden", to = "Gregory Nava", amount = 315, tid = "ec8be07c-7dc5-4ee0-a84b-f83f1bdec8b9"},Transaction {from = "Angeline Keck", to = "Shaanan Cohney", amount = 495, tid = "4989bb16-9a09-479d-b776-9c0fd38bbd4f"},Transaction {from = "Una Nobles", to = "Shanta Shafer", amount = 176, tid = "b0e96f56-fd03-4034-a64c-22bf915dbdfa"},Transaction {from = "Genaro Goins", to = "Shaanan Cohney", amount = 97, tid = "6f94c0cb-574b-48c5-8fc0-9a698f7be9be"},Transaction {from = "Santos Maples", to = "Shaanan Cohney", amount = 517, tid = "d808d2f7-791f-4083-9cd3-b6f3e699880f"},Transaction {from = "Lucie Yazzie", to = "Allen Delagarza", amount = 106, tid = "b9950910-9b22-4441-8bcd-de10ccc52de2"},Transaction {from = "Louie Lovelace", to = "Shaanan Cohney", amount = 581, tid = "b18e6096-36e6-42f8-911f-c1588e40e07f"},Transaction {from = "Shanti Hanley", to = "Colette Pickering", amount = 82, tid = "8f44e282-b816-4183-aeb8-7da9281fc8f9"},Transaction {from = "Candis Lemus", to = "Lennie Haight", amount = 96, tid = "e312ca0a-38f8-46b8-8721-be57b876c749"},Transaction {from = "Jule Booker", to = "Shaanan Cohney", amount = 173, tid = "5cb1fd10-0821-4c10-9d51-fc7ba6c36ac8"},Transaction {from = "Teddy Cheng", to = "Virgen Herman", amount = 34, tid = "7a861c8f-e7bc-4d1f-921b-cf32d62f30dc"},Transaction {from = "Brenna Woodall", to = "Marlyn Pulido", amount = 149, tid = "738ca80e-2673-4d21-86f9-4d491e67e3ac"},Transaction {from = "Jami Kuntz", to = "Shaanan Cohney", amount = 455, tid = "5f6873d4-ce68-4e37-ab8b-9c87450200bc"},Transaction {from = "Aisha Craft", to = "Shaanan Cohney", amount = 27, tid = "4a556757-5b4a-4d9b-8aba-187a2645d2ac"},Transaction {from = "Sharice Bledsoe", to = "Joanie Cramer", amount = 141, tid = "5882409e-050d-4bab-b376-18888293598f"},Transaction {from = "Leia Klinger", to = "Andre Cummins", amount = 206, tid = "016df92c-4fec-4d26-b3b0-a7994b6a28e4"},Transaction {from = "Charise Bradford", to = "Shaanan Cohney", amount = 595, tid = "91d8e817-933a-4111-9d75-d454648c8f27"},Transaction {from = "Domenica Gable", to = "Felipa Sallee", amount = 255, tid = "0d813620-4172-44db-a247-c54dc6c4bc35"},Transaction {from = "Angelika Batson", to = "Shaanan Cohney", amount = 126, tid = "f1a26fde-8a3a-4330-ba8c-dc7955c064ad"},Transaction {from = "Maurice Comer", to = "Shaanan Cohney", amount = 43, tid = "3e56fcb1-a9ff-4653-8263-79dd338b18cf"},Transaction {from = "Elana Holden", to = "Mollie Halverson", amount = 16, tid = "d72752dd-be79-402b-9f38-bc4c428c92c4"},Transaction {from = "Gregory Nava", to = "Shaanan Cohney", amount = 550, tid = "10a2edd4-ede8-4b5f-adf8-9976a11042ac"},Transaction {from = "Jami Kuntz", to = "Charise Bradford", amount = 232, tid = "6f0ef2e2-8ff0-4bf9-9602-e700f2b88685"},Transaction {from = "Jason Mccallister", to = "Lissette Epperson", amount = 24, tid = "0aea7d84-6686-471c-8d37-eafa52498062"},Transaction {from = "Elana Holden", to = "Hayden Weatherford", amount = 188, tid = "19e065d9-8170-43ce-a74e-8678521bbc6a"},Transaction {from = "Reta Salcedo", to = "Faustina Wolford", amount = 367, tid = "8ed6ebf4-c19a-480b-b18a-585c3476ae46"},Transaction {from = "Felipa Sallee", to = "Valarie Hoy", amount = 229, tid = "90f22457-6d7f-455c-bf42-4f38c10cbadd"},Transaction {from = "Alyson Pinkston", to = "Gregory Nava", amount = 230, tid = "3767ef92-c8f0-4e18-8436-78e903687bdc"},Transaction {from = "Elana Holden", to = "Allen Delagarza", amount = 115, tid = "8baaa1d6-834f-40c8-9559-407bc0b216f8"},Transaction {from = "Alyson Pinkston", to = "Charise Bradford", amount = 92, tid = "d2f0ad42-e827-4290-a8a1-d3d380ff5ac3"},Transaction {from = "Lennie Haight", to = "Shaunte Foy", amount = 285, tid = "52372fae-0e70-4e77-b611-ffc4e48cfaa9"},Transaction {from = "Farah Bateman", to = "Margurite Thorn", amount = 128, tid = "d7acfed5-f2fb-4967-9af3-993f87a1d55b"},Transaction {from = "Sharice Bledsoe", to = "Santos Maples", amount = 341, tid = "d115b9c4-30cd-43f3-91b1-72d1ebd7cf67"},Transaction {from = "Nannie Swisher", to = "Shaanan Cohney", amount = 23, tid = "c45f38bc-5055-4733-9d74-70b04c01e3f6"},Transaction {from = "Mollie Halverson", to = "Jeanetta Hoyt", amount = 252, tid = "ff790702-a78c-42b6-9daf-1e019ffd202f"},Transaction {from = "Robby Laughlin", to = "Colette Pickering", amount = 93, tid = "056b2cbe-fbd2-4a1d-bdf2-0b3e8a946508"},Transaction {from = "Jason Mccallister", to = "Teddy Cheng", amount = 39, tid = "9b641890-6a1a-462f-be74-58d529066ebb"},Transaction {from = "Dia Monson", to = "Bettye Wooten", amount = 228, tid = "e7ef4fe0-b119-4983-a996-46783d0a123c"},Transaction {from = "Newton Burks", to = "Altha Redden", amount = 129, tid = "90ab63d8-bddf-485b-92bc-600fff3dbe11"},Transaction {from = "Maurice Comer", to = "Khalilah Stout", amount = 137, tid = "5951b7d9-aa33-490f-8d26-337475dadf01"},Transaction {from = "Allen Delagarza", to = "Torie Martino", amount = 230, tid = "8201b900-9b12-47ca-a9ab-280e8d32bd2b"},Transaction {from = "Dulce Arnett", to = "Shaanan Cohney", amount = 362, tid = "d776eebc-fefd-4b7c-b46f-5095e8415295"},Transaction {from = "Margeret Scully", to = "Bettye Wooten", amount = 295, tid = "da72aed7-15ed-428e-9bdf-82e1ea99256d"},Transaction {from = "Jeanetta Hoyt", to = "Shaanan Cohney", amount = 1252, tid = "2cc5712d-0609-48b2-ba05-f617a850a5a4"},Transaction {from = "Candis Lemus", to = "Ozella Burkett", amount = 124, tid = "9041f057-e7ad-450e-b7ff-b7abd4c7c625"},Transaction {from = "Newton Burks", to = "Sharice Bledsoe", amount = 278, tid = "3cfd1be0-373f-4725-95be-feb3dfb91e0d"},Transaction {from = "Ignacia Baugh", to = "Genaro Goins", amount = 155, tid = "4638db8d-19d4-4a75-8e16-3527f5de3ee8"},Transaction {from = "Matilda Albright", to = "Louie Lovelace", amount = 63, tid = "5e910b13-537c-47ef-a4a9-c46d3fe0d7ff"},Transaction {from = "August Jewell", to = "Dorsey Gore", amount = 357, tid = "efff03a0-cda8-440d-9f19-abc01996dabc"},Transaction {from = "Dona Link", to = "Maurice Comer", amount = 180, tid = "5d5cb8d4-0197-4e27-b1ec-29399ed06ca5"},Transaction {from = "Bettye Wooten", to = "Santo Sutherland", amount = 147, tid = "830d6442-b15b-4596-a7ec-7000652c7981"},Transaction {from = "Jerrie Breedlove", to = "Shaanan Cohney", amount = 296, tid = "3f7bce72-49bb-4c01-a0b1-ae2af7c8ae76"},Transaction {from = "Alessandra Blackmon", to = "Shaanan Cohney", amount = 263, tid = "cb8395ac-f998-4e92-b292-084bfaa7a667"},Transaction {from = "Kia Burkholder", to = "Dulce Arnett", amount = 362, tid = "1e94a945-1475-46e3-94f0-31afd587425c"},Transaction {from = "Ozella Burkett", to = "Shaanan Cohney", amount = 400, tid = "58182389-88c0-4650-890a-b62b5261181b"},Transaction {from = "Daria Hawk", to = "Jeanetta Hoyt", amount = 370, tid = "e0aa8ba2-bae7-4639-ad18-bd39a31e99d5"},Transaction {from = "Domenica Gable", to = "Joanie Cramer", amount = 197, tid = "2599f341-1d45-4f0e-acce-9afaf8fe8232"},Transaction {from = "Alvera Barrera", to = "Alyson Pinkston", amount = 166, tid = "0b439017-0979-4a88-aa2f-b2d8d4583d22"},Transaction {from = "Teodoro Gold", to = "Dona Link", amount = 14, tid = "ae00bcec-8f92-46eb-b3ee-e5c00d0acef4"},Transaction {from = "Elana Holden", to = "Dia Monson", amount = 96, tid = "1ff19026-dd2d-412f-bb3c-00b8ea8c1a78"},Transaction {from = "Edris Corbitt", to = "Dorsey Gore", amount = 130, tid = "7c3045c8-58b3-4e24-8b6c-638739483389"},Transaction {from = "Felecia Mckeown", to = "Brenna Woodall", amount = 373, tid = "a0e4f6ed-c292-449d-9e69-35d6a4192e48"},Transaction {from = "Alvera Barrera", to = "Jerrie Breedlove", amount = 296, tid = "d6fd7b95-89b7-41f6-bd94-a6df3bf7a03d"},Transaction {from = "Una Nobles", to = "Farah Bateman", amount = 107, tid = "63f7ea0b-5b7d-4b4c-bdc4-8b57f7582be9"},Transaction {from = "Virgen Herman", to = "Bettye Wooten", amount = 20, tid = "f82f5ac9-f50a-4db0-9ae6-e33bcc8d1611"},Transaction {from = "Ngan Bratcher", to = "August Jewell", amount = 40, tid = "42be9452-c246-41c6-b0ee-4b0660f7388a"},Transaction {from = "Alyson Pinkston", to = "Roscoe Stamps", amount = 236, tid = "2d26b3e0-9bd3-46f1-9ae1-3576ed3f5db8"},Transaction {from = "Ileana Broughton", to = "Shaanan Cohney", amount = 74, tid = "0bcc4f20-0a82-4c2b-afe8-4bf943f168b8"},Transaction {from = "Angeline Keck", to = "August Jewell", amount = 69, tid = "28f06e76-baf0-4a14-b982-c26b2b857ade"},Transaction {from = "Charlyn Keefer", to = "Shaanan Cohney", amount = 175, tid = "2baa7298-a085-4428-9010-4b2195c09fc3"},Transaction {from = "Margeret Scully", to = "Leia Klinger", amount = 28, tid = "54091dc2-a749-443f-bf66-a78b2f363512"},Transaction {from = "Colette Pickering", to = "Jami Kuntz", amount = 342, tid = "24e8faa3-b691-47fc-8740-93459e760de9"},Transaction {from = "Altha Redden", to = "Angeline Keck", amount = 25, tid = "83056c1b-4a7e-443a-bd92-46f5de42d214"},Transaction {from = "Allen Delagarza", to = "Sharice Bledsoe", amount = 93, tid = "6c97d71d-d406-4ab2-a22f-bff530b82bee"},Transaction {from = "Lucie Yazzie", to = "Shaanan Cohney", amount = 107, tid = "81656cb1-ecc2-4901-ab65-d19e59324c08"},Transaction {from = "JC Casas", to = "Bettye Wooten", amount = 360, tid = "45db2fca-8a65-4539-b194-81ab5c539be9"},Transaction {from = "Joanie Cramer", to = "Lennie Haight", amount = 351, tid = "61ce81a1-cbde-45bf-a95b-a7c09177baf0"},Transaction {from = "Virgen Herman", to = "Shaanan Cohney", amount = 14, tid = "4abcd4c8-ed57-4243-a74b-4cfe6a8743b2"},Transaction {from = "Colette Pickering", to = "Ericka Beverly", amount = 190, tid = "2b7f02e1-817a-470f-8c43-ddbc291229c1"},Transaction {from = "Felipa Sallee", to = "Aisha Craft", amount = 60, tid = "c3293dcc-52e5-473c-9041-70d66d3fe213"},Transaction {from = "Khalilah Stout", to = "Newton Burks", amount = 361, tid = "00a81989-e748-4587-bd01-a0f8e175aaa9"},Transaction {from = "Angelika Batson", to = "Suellen Boatwright", amount = 98, tid = "c3ede89c-41a6-4a7a-8da4-ba9254510847"},Transaction {from = "Sharice Bledsoe", to = "Shaanan Cohney", amount = 16, tid = "27f6edff-02b3-47e7-8701-03ae4683a46c"},Transaction {from = "Santo Sutherland", to = "Shaanan Cohney", amount = 153, tid = "8130e2fc-cec9-4f63-9fb7-3166800885c9"},Transaction {from = "Shanti Hanley", to = "Bettye Wooten", amount = 87, tid = "e032d77e-1091-49df-9546-4b8d982c27f0"},Transaction {from = "Allen Delagarza", to = "Blondell See", amount = 280, tid = "9a6318a3-f463-4111-a33e-b199f1eeddf9"},Transaction {from = "Ngan Bratcher", to = "Charlyn Keefer", amount = 117, tid = "ad1cc998-255a-4124-abd2-0879e75d3182"},Transaction {from = "Marlyn Pulido", to = "Shaanan Cohney", amount = 149, tid = "45b9c594-6f9f-48fe-9708-cdf57aa033c3"},Transaction {from = "Carline Epstein", to = "Santo Sutherland", amount = 29, tid = "ce7b3a30-6d23-440e-900a-955c065d9953"},Transaction {from = "Carline Epstein", to = "Shaanan Cohney", amount = 276, tid = "23f52e71-4efa-43ec-84c0-c9bb3e355c9a"},Transaction {from = "Cherie Mccool", to = "Shaanan Cohney", amount = 379, tid = "23db198f-22c6-407c-baa9-1c6afffc3a5c"},Transaction {from = "Newton Burks", to = "Teodoro Gold", amount = 204, tid = "8c70bb20-c673-4777-be29-e72f725fd004"},Transaction {from = "Santo Sutherland", to = "Margeret Scully", amount = 23, tid = "988937a7-2449-4278-a3a9-034fa357f09b"},Transaction {from = "Dorsey Gore", to = "Shaanan Cohney", amount = 74, tid = "92adad57-d3ab-44e1-b0cb-1c1acb682028"},Transaction {from = "Dona Link", to = "Colette Pickering", amount = 259, tid = "6c8bf6e3-3bde-4f3e-b903-a8b1c06e3797"},Transaction {from = "Aisha Craft", to = "Ileana Broughton", amount = 96, tid = "7b29fa91-1872-4181-9fb5-21e5c79c39de"},Transaction {from = "Francie Duggan", to = "Carline Epstein", amount = 305, tid = "be3ab544-2889-4b73-9737-f73f46f1d87b"},Transaction {from = "Lissette Epperson", to = "Blondell See", amount = 59, tid = "dcbff568-2a5b-491b-8759-a6ebde9b223b"},Transaction {from = "Hayden Weatherford", to = "Shaunte Foy", amount = 377, tid = "a49083ae-d989-4532-8b75-260ed126de7c"},Transaction {from = "Valarie Hoy", to = "Elana Holden", amount = 377, tid = "43e44b76-22e3-4f76-94db-73f13023856b"},Transaction {from = "Ignacia Baugh", to = "Heike Champagne", amount = 95, tid = "92a44203-efb8-4c09-95ef-b65bba5aeae8"},Transaction {from = "Valarie Hoy", to = "Elana Holden", amount = 50, tid = "b8980e1f-e726-44e2-bdc7-93e488d3c2d7"},Transaction {from = "Alyson Pinkston", to = "Blondell See", amount = 61, tid = "6142fee0-9552-4bea-815a-1ce92ff5aaeb"},Transaction {from = "Bettye Wooten", to = "Shaanan Cohney", amount = 663, tid = "4fa89659-ff12-45b7-a45c-85a418e3171f"},Transaction {from = "Jennefer Eggleston", to = "Bettye Wooten", amount = 116, tid = "ca64691f-df6e-44b9-bdad-15f9df15268d"},Transaction {from = "Dorsey Gore", to = "Altha Redden", amount = 354, tid = "ed0c3409-0b73-417a-8583-e48b85e2c2d8"},Transaction {from = "Leia Klinger", to = "Faustina Wolford", amount = 28, tid = "79ede0a9-01da-4f37-acb1-7ef3af5655ce"},Transaction {from = "Heike Champagne", to = "Charise Bradford", amount = 271, tid = "439fff85-c1cc-445b-88ff-fabcce76159a"},Transaction {from = "Ngan Bratcher", to = "Angelika Batson", amount = 224, tid = "7cd86381-d4f3-465a-a4a7-4a708bcd4aed"},Transaction {from = "August Jewell", to = "Shaunte Foy", amount = 179, tid = "7aca715c-6c78-4acb-a630-52ab0856a227"},Transaction {from = "Ignacia Baugh", to = "Andre Cummins", amount = 352, tid = "e24d16e0-df1b-48c1-bb1e-ce75161158fa"},Transaction {from = "Aisha Craft", to = "Jule Booker", amount = 173, tid = "5330265c-86cb-462b-94dc-32500998130e"},Transaction {from = "Lennie Haight", to = "Shaanan Cohney", amount = 69, tid = "6904795e-c589-4c97-9af4-f90b7329da26"},Transaction {from = "Lennie Haight", to = "Hayden Weatherford", amount = 148, tid = "e9e7ef23-6a2f-488d-a936-a86491672432"},Transaction {from = "Lissette Epperson", to = "Caprice Burnett", amount = 243, tid = "2d391fa3-12b7-46e4-8c6a-bd8a13cf4c3f"},Transaction {from = "Andre Cummins", to = "Shaanan Cohney", amount = 558, tid = "adb7c21d-7430-44ee-8edf-dc8d08ebf79b"},Transaction {from = "Ignacia Baugh", to = "Lennie Haight", amount = 55, tid = "afd46d94-fc3c-4148-950f-19190b89520a"},Transaction {from = "Jeraldine Williford", to = "Sharice Bledsoe", amount = 127, tid = "1d5ee014-c18a-4962-8788-4d0b6092488f"},Transaction {from = "Newton Burks", to = "Gregory Nava", amount = 115, tid = "741f70c3-e644-469d-a5a1-d40aebc04243"},Transaction {from = "Francie Duggan", to = "Shanti Hanley", amount = 62, tid = "95e82b3c-6874-40fa-9848-0813b9c04280"},Transaction {from = "Santos Maples", to = "Lissette Epperson", amount = 175, tid = "6a24a4bf-d570-4f49-91ab-4c53b4bc227e"}]

una :: [Transaction]
una = [Transaction {from = "Una Nobles", to = "Leia Klinger", amount = 89, tid = "7bc3b57b-70cb-4f24-90e2-12dc71227238"},Transaction {from = "Una Nobles", to = "Dorsey Gore", amount = 34, tid = "23288493-e2fb-4bef-9306-83667f1efe06"},Transaction {from = "Una Nobles", to = "Shanta Shafer", amount = 176, tid = "b0e96f56-fd03-4034-a64c-22bf915dbdfa"},Transaction {from = "Una Nobles", to = "Farah Bateman", amount = 107, tid = "63f7ea0b-5b7d-4b4c-bdc4-8b57f7582be9"},Transaction {from = "Genaro Goins", to = "Una Nobles", amount = 168, tid = "bb94b7c7-75b7-4610-bf85-299b06c8ed5a"}]

-- Helpers --------------------------------------------

xorByteStringsWithFilter :: (Word8 -> Bool) -> ByteString -> ByteString -> ByteString
xorByteStringsWithFilter predicate bs1 bs2 = BS.pack $ filter (predicate) $ BS.zipWith (xor) bs1 bs2

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origFileName modFileName = do
  bsOrig <- BS.readFile origFileName
  bsMod <- BS.readFile modFileName
  return $ xorByteStringsWithFilter (/=0) bsOrig bsMod

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encrypted <- BS.readFile (path ++ ".enc")
  BS.writeFile path $ xorByteStringsWithFilter (const True) encrypted (BS.cycle key)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = BS.readFile path >>= return . decode

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transPath = do
  mvs <- parseFile victimsPath
  mts <- parseFile transPath
  return $ bad <$> mvs <*> mts
  where bad vs ts = filter ((`elem` vs) . tid) ts

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldl (build) Map.empty ts
  where build m (Transaction { from = giver
                             , to = receiver
                             , amount = amt }) = upsert (+) giver (amt * (-1)) $ upsert (+) receiver amt m

upsert :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
upsert updater k a m = case Map.lookup k m of
  Just _ -> Map.adjust (updater a) k m
  Nothing -> Map.insert k a m

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldlWithKey (go) ("Nobody", 0)
  where go p@(_, biggestAmt) person amt = if (amt > biggestAmt) then (person, amt) else p

-- Exercise 7 -----------------------------------------

setId :: TId -> Transaction -> Transaction
setId tid t = Transaction (from t) (to t) (amount t) tid

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ids = zipWith (setId) ids (personAmtsToTrans $ Map.toList m)

personAmtsToTrans :: [(String, Integer)] -> [Transaction]
personAmtsToTrans [] = []
personAmtsToTrans ps = ts ++ personAmtsToTrans remainder
  where (remainder, ts) = personAmtsToTransWithRem ps

sortPeopleByAmt :: Bool -> [(String, Integer)] -> [(String, Integer)]
sortPeopleByAmt asc ps = L.sortBy (\x y -> (if asc then compare else flip compare) (snd x) (snd y)) ps

-- payers: those who ended up with positive balance
-- payees: those who ended up with negative balance

payers :: [(String, Integer)] -> [(String, Integer)]
payers = sortPeopleByAmt False . filter ((>0) . snd)

payees :: [(String, Integer)] -> [(String, Integer)]
payees = sortPeopleByAmt False . filter ((<0) . snd)

zip2WithPad :: (a, b) -> [(a, b)] -> [(a, b)] -> [((a, b), (a, b))]
zip2WithPad pad (x:xs) (y:ys) = (x, y)  :(zip2WithPad pad xs ys)
zip2WithPad pad []     (x:xs) = (pad, x):(zip2WithPad pad [] xs)
zip2WithPad pad (x:xs) []     = (x, pad):(zip2WithPad pad xs [])
zip2WithPad _ _ _             = []

personAmtsToTransWithRem :: [(String, Integer)] -> ([(String, Integer)], [Transaction])
personAmtsToTransWithRem (_:[]) = ([], [])
personAmtsToTransWithRem ps = foldl (transact) ([], []) $ zip2WithPad ("ignore", 0) (payees ps) (payers ps)
  where transact (ps', ts) (("ignore", 0), n)             = (n:ps', ts)
        transact (ps', ts) (p, ("ignore", 0))             = (p:ps', ts)
        transact (ps', ts) ((pye, pyeAmt), (pyr, pyrAmt)) = case compare (abs pyeAmt) pyrAmt of
                                                             GT -> ((pye, pyrAmt+pyeAmt):ps', (Transaction pyr pye pyrAmt "replace"):ts) -- was owed more than had to give
                                                             LT -> ((pyr, pyrAmt+pyeAmt):ps', (Transaction pyr pye pyrAmt "replace"):ts) -- had to give more than was owed
                                                             EQ -> (ps',                      (Transaction pyr pye pyeAmt "replace"):ts) -- cancel each other out!

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp x = BS.writeFile fp (encode x)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
