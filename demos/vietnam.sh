#!/bin/sh

# Where is LFS mounted?
LFS_ROOT=`pwd`

#if [ $# -ne 1 ]
#    then echo "Usage: $0 <LFS mountpoint>"
#    exit
#fi



# Creating taxonomy
# ingredients

mkdir $LFS_ROOT/ingredient
mkdir $LFS_ROOT/ingredient/sea-food
mkdir $LFS_ROOT/ingredient/sea-food/crab
mkdir $LFS_ROOT/ingredient/sea-food/shrimp
mkdir $LFS_ROOT/ingredient/sea-food/cuttlefish
mkdir $LFS_ROOT/ingredient/sea-food/jellyfish
mkdir $LFS_ROOT/ingredient/sea-food/shell
mkdir $LFS_ROOT/ingredient/sea-food/shell/abalone
mkdir $LFS_ROOT/ingredient/sea-food/shell/bulots
mkdir $LFS_ROOT/ingredient/sea-food/fish
mkdir $LFS_ROOT/ingredient/sea-food/fish/eel
mkdir $LFS_ROOT/ingredient/sea-food/fish/coley
mkdir $LFS_ROOT/ingredient/sea-food/fish/sea_bream
mkdir $LFS_ROOT/ingredient/sea-food/fish/monkfish
mkdir $LFS_ROOT/ingredient/sea-food/fish/tuna

# bulots ?
# abalone ?
# colin -> coley (br) , pollock (Am)
# lotte -> burbot (rivière), monkfish (de mer)

mkdir $LFS_ROOT/ingredient/other
mkdir $LFS_ROOT/ingredient/other/cu_nang
mkdir $LFS_ROOT/ingredient/other/dau_xanh
mkdir $LFS_ROOT/ingredient/other/lotus
mkdir $LFS_ROOT/ingredient/other/mam_tom
mkdir $LFS_ROOT/ingredient/other/ram_on
mkdir $LFS_ROOT/ingredient/other/ram_hun
mkdir $LFS_ROOT/ingredient/other/tapioca
mkdir $LFS_ROOT/ingredient/other/agar_agar
mkdir $LFS_ROOT/ingredient/other/rice_alcohol
mkdir $LFS_ROOT/ingredient/other/carmin
mkdir $LFS_ROOT/ingredient/other/glutamate
mkdir $LFS_ROOT/ingredient/other/soya_alcohol

# carmin ?
# glutamate ?

mkdir $LFS_ROOT/ingredient/spice
mkdir $LFS_ROOT/ingredient/spice/sweet
mkdir $LFS_ROOT/ingredient/spice/sweet/anis_etoile
mkdir $LFS_ROOT/ingredient/spice/sweet/vanille
mkdir $LFS_ROOT/ingredient/spice/sweet/badiane
mkdir $LFS_ROOT/ingredient/spice/mild
mkdir $LFS_ROOT/ingredient/spice/mild/5parfums
mkdir $LFS_ROOT/ingredient/spice/mild/curry
mkdir $LFS_ROOT/ingredient/spice/mild/gingembre
mkdir $LFS_ROOT/ingredient/spice/mild/curcuma
mkdir $LFS_ROOT/ingredient/spice/mild/safran
mkdir $LFS_ROOT/ingredient/spice/mild/tamarin
mkdir $LFS_ROOT/ingredient/spice/fresh
mkdir $LFS_ROOT/ingredient/spice/fresh/citronnelle
mkdir $LFS_ROOT/ingredient/spice/fresh/ciboule
mkdir $LFS_ROOT/ingredient/spice/fresh/coriandre
mkdir $LFS_ROOT/ingredient/spice/fresh/mint
mkdir $LFS_ROOT/ingredient/spice/fresh/persicaire

mkdir $LFS_ROOT/ingredient/sauce
mkdir $LFS_ROOT/ingredient/sauce/nuoc_mam
mkdir $LFS_ROOT/ingredient/sauce/sauce_huitre
mkdir $LFS_ROOT/ingredient/sauce/sauce_soja

mkdir $LFS_ROOT/ingredient/fruit
mkdir $LFS_ROOT/ingredient/fruit/carambole
mkdir $LFS_ROOT/ingredient/fruit/coconut
mkdir $LFS_ROOT/ingredient/fruit/apple
mkdir $LFS_ROOT/ingredient/fruit/banana
mkdir $LFS_ROOT/ingredient/fruit/ananas
mkdir $LFS_ROOT/ingredient/fruit/lychee
mkdir $LFS_ROOT/ingredient/fruit/longane
mkdir $LFS_ROOT/ingredient/fruit/orange

mkdir $LFS_ROOT/ingredient/vegetable
mkdir $LFS_ROOT/ingredient/vegetable/mushroom
mkdir $LFS_ROOT/ingredient/vegetable/chou_sale
mkdir $LFS_ROOT/ingredient/vegetable/pousse_bambou
mkdir $LFS_ROOT/ingredient/vegetable/navet
mkdir $LFS_ROOT/ingredient/vegetable/carotte
mkdir $LFS_ROOT/ingredient/vegetable/concombre
mkdir $LFS_ROOT/ingredient/vegetable/celeri
mkdir $LFS_ROOT/ingredient/vegetable/marron_d_eau
mkdir $LFS_ROOT/ingredient/vegetable/tomato
mkdir $LFS_ROOT/ingredient/vegetable/asperge
mkdir $LFS_ROOT/ingredient/vegetable/chou_blanc
mkdir $LFS_ROOT/ingredient/vegetable/chou_vert
mkdir $LFS_ROOT/ingredient/vegetable/corn
mkdir $LFS_ROOT/ingredient/vegetable/salade
mkdir $LFS_ROOT/ingredient/vegetable/petit_pois
mkdir $LFS_ROOT/ingredient/vegetable/pruneau
mkdir $LFS_ROOT/ingredient/vegetable/epinard
mkdir $LFS_ROOT/ingredient/vegetable/haricot_vert
mkdir $LFS_ROOT/ingredient/vegetable/haricot_blanc
mkdir $LFS_ROOT/ingredient/vegetable/rice
#mkdir $LFS_ROOT/ingredient/vegetable/rice/riz
mkdir $LFS_ROOT/ingredient/vegetable/rice/glutinous_rice
mkdir $LFS_ROOT/ingredient/vegetable/rice/farine_riz
mkdir $LFS_ROOT/ingredient/vegetable/rice/galette_riz
mkdir $LFS_ROOT/ingredient/vegetable/rice/pate_riz
mkdir $LFS_ROOT/ingredient/vegetable/rice/vermicelle

mkdir $LFS_ROOT/ingredient/vegetable/soya
mkdir $LFS_ROOT/ingredient/vegetable/soya/crepe_soja
mkdir $LFS_ROOT/ingredient/vegetable/soya/tofu
mkdir $LFS_ROOT/ingredient/vegetable/soya/soya_cheese
mkdir $LFS_ROOT/ingredient/vegetable/soya/soya_bean_sprout
mkdir $LFS_ROOT/ingredient/vegetable/soya/soya_bean
mkdir $LFS_ROOT/ingredient/vegetable/soya/soya_sale
mkdir $LFS_ROOT/ingredient/vegetable/soya/green_soya
mkdir $LFS_ROOT/ingredient/vegetable/soya/pate_soja

mkdir $LFS_ROOT/ingredient/meat
mkdir $LFS_ROOT/ingredient/meat/gibier
mkdir $LFS_ROOT/ingredient/meat/pork
#mkdir $LFS_ROOT/ingredient/meat/pork/porc
mkdir $LFS_ROOT/ingredient/meat/pork/porc_pile
mkdir $LFS_ROOT/ingredient/meat/pork/jambon
mkdir $LFS_ROOT/ingredient/meat/pork/saucisse_chinoise
mkdir $LFS_ROOT/ingredient/meat/poultry
mkdir $LFS_ROOT/ingredient/meat/poultry/poulet
mkdir $LFS_ROOT/ingredient/meat/poultry/volaille
mkdir $LFS_ROOT/ingredient/meat/poultry/oeuf
mkdir $LFS_ROOT/ingredient/meat/beef
#mkdir $LFS_ROOT/ingredient/meat/beef/boeuf
mkdir $LFS_ROOT/ingredient/meat/veau


mkdir $LFS_ROOT/vegetarien

mkdir $LFS_ROOT/meal

mkdir $LFS_ROOT/meal/starter
mkdir $LFS_ROOT/meal/starter/nem
mkdir $LFS_ROOT/meal/salad
mkdir $LFS_ROOT/meal/soup
mkdir $LFS_ROOT/meal/main
mkdir $LFS_ROOT/meal/fondue
mkdir $LFS_ROOT/meal/pasta
mkdir $LFS_ROOT/meal/dessert
mkdir $LFS_ROOT/meal/drink
mkdir $LFS_ROOT/meal/drink/the
mkdir $LFS_ROOT/meal/plat-complet

mkdir $LFS_ROOT/menu
mkdir $LFS_ROOT/menu/standard
mkdir $LFS_ROOT/menu/standard/menu1
mkdir $LFS_ROOT/menu/standard/menu2
mkdir $LFS_ROOT/menu/standard/menu3
mkdir $LFS_ROOT/menu/standard/menu4
mkdir $LFS_ROOT/menu/gourmet
mkdir $LFS_ROOT/menu/menu_I
mkdir $LFS_ROOT/menu/menu_II
mkdir $LFS_ROOT/menu/menu_III




# mkdir le_chant_du_riz_pile
# mkdir cent_recettes_vietnamiennes
# mkdir L_Harmattan
# cd le_chant_du_riz_pile/cent_recettes_vietnamiennes/L_Harmattan

# les entrees


# starters
echo "Paté impérial à la vietnamienne" >> $LFS_ROOT/starter/vermicelle/soya_bean_sprout/mushroom/shrimp/glutamate/galette_riz/nem/Cha_gio
echo "Crèpe de Hué farcie" >> $LFS_ROOT/starter/farine_riz/ciboule/safran/pork/mushroom/shrimp/soya_bean_sprout/Banh_xeo_Hue
echo "Omelette au crabe ou aux crevettes" >> $LFS_ROOT/starter/shrimp/ciboule/coriandre/Hot_ga_chien_cua
echo "Patés de crabe ou de crevettes" >> $LFS_ROOT/starter/pork/vermicelle/mushroom/shrimp/glutamate/oeuf/ciboule/Cha_cua_hay_tom
echo "Gateaux roulés à la vapeur" >> $LFS_ROOT/starter/farine_riz/pork/shrimp/mushroom/Banh_cuon
echo "Rouleaux de printemps aux crevettes" >> $LFS_ROOT/starter/rice/vermicelle/pork/shrimp/ciboule/mint/coriandre/soya_bean_sprout/Goi_cuon

# pasta
echo "Plats à base de vermicelle de riz" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/Bun
echo "Vermicelle de riz au boeuf" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/beef/citronnelle/Bo_bun
echo "Vermicelle de riz au porc" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/citronnelle/Heo_bun
echo "Vermicelle de riz au porc émincé" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/rice/Bi_bun
echo "Vermicelle de riz végétarien" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/soya_cheese/sauce_soja/vegetarien/Bun
echo "Rouleaux de printemps vegetariens" >> $LFS_ROOT/pasta/soya_bean_sprout/vermicelle/pork/mint/coriandre/soya_cheese/crepe_soja/rice/Goi_cuon

# salads
echo "Salade de méduse" >> $LFS_ROOT/salad/concombre/carotte/navet/soya_bean_sprout/jellyfish/pork/oeuf/mint/nuoc_mam/Nom_sua
echo "Salade de soja" >> $LFS_ROOT/salad/soya_bean_sprout/Goi_gia
echo "Salade de soja (variante)" >> $LFS_ROOT/salad/soya_bean_sprout/shrimp/mint/celeri/carotte/nuoc_mam/Goi_gia

# soups
echo "Soupe Hanoienne" >> $LFS_ROOT/soup/beef/shrimp/cuttlefish/gingembre/anis_etoile/glutamate/nuoc_mam/pate_riz/mint/coriandre/Pho
echo "Soupe de vermicelles aux bulots" >> $LFS_ROOT/soup/bulots/tomato/glutamate/nuoc_mam/vermicelle/coriandre/Bun_oc
echo "Potage aux choux salés" >> $LFS_ROOT/soup/pork/oeuf/vermicelle/chou_sale/glutamate/Canh_cai_bac_thao
echo "Potage aux pointes d'asperges et au crabe" >> $LFS_ROOT/soup/crab/asperge/glutamate/oeuf/coriandre/ciboule/Mang_cua
echo "Soupe" >> $LFS_ROOT/soup/jambon/shrimp/nuoc_mam/glutamate/rice/ciboule/coriandre/Ban_canh
echo "Soupe de pied de veau au riz" >> $LFS_ROOT/soup/veau/shrimp/pork/rice/ciboule/nuoc_mam/coriandre/Chao_chan_bo
echo "Soupe de canard au vermicelle" >> $LFS_ROOT/soup/volaille/glutamate/safran/pousse_bambou/nuoc_mam/vermicelle/ciboule/Vit_mieng
echo "Soupe de boeuf au tapioca" >> $LFS_ROOT/soup/beef/oeuf/nuoc_mam/glutamate/ciboule/coriandre/Chao_bot_ban
echo "Soupe au poulet" >> $LFS_ROOT/soup/volaille/rice/chou_blanc/celeri/ciboule/nuoc_mam/glutamate/Chao_ga
echo "Soupe au canard" >> $LFS_ROOT/soup/volaille/rice/ciboule/nuoc_mam/glutamate/gingembre/Chao_vit
echo "Soupe aux abalones" >> $LFS_ROOT/soup/rice/pork/ciboule/nuoc_mam/glutamate/abalone/Chao_bao_ngu
echo "Soupe au riz et au porc" >> $LFS_ROOT/soup/rice/pork/ciboule/Chao_heo
echo "Soupe de poisson au riz" >> $LFS_ROOT/soup/rice/sea-food/gingembre/ciboule/coriandre/glutamate/oeuf/Chao_ca
echo "Potage aigre-doux au poisson" >> $LFS_ROOT/soup/tamarin/nuoc_mam/shrimp/tomato/celeri/soya_bean_sprout/mint/Canh_chua


# main: fish and sea-food
echo "Anguilles en brochettes" >> $LFS_ROOT/main/eel/nuoc_mam/gingembre/safran/glutamate/Cha_luong
#echo "Poisson au gingembre" >> $LFS_ROOT/main/mushroom_parfume/gingembre/pousse_bambou/sauce_soja/glutamate/pork/ciboule/Ca_nau_nung
echo "Poisson au gingembre" >> $LFS_ROOT/main/mushroom/gingembre/pousse_bambou/sauce_soja/glutamate/pork/ciboule/Ca_nau_nung
echo "Anguille sautee au vermicelle chinois" >> $LFS_ROOT/main/eel/nuoc_mam/vermicelle/glutamate/ciboule/Luong_xao_mieng
#echo "Anguille a la vapeur" >> $LFS_ROOT/main/eel/pork/vermicelle/mushroom/coconut/ran_om/Luong_um
echo "Anguille a la vapeur" >> $LFS_ROOT/main/eel/pork/vermicelle/mushroom/coconut/Luong_um
#echo "Poisson farci" >> $LFS_ROOT/main/coley/pork/ciboule/champigon_noir/gingembre/sauce_soja/Ca_nhoi
echo "Poisson farci" >> $LFS_ROOT/main/coley/pork/ciboule/mushroom/gingembre/sauce_soja/Ca_nhoi
echo "Poisson a la citronnelle" >> $LFS_ROOT/main/sea_bream/citronnelle/Ca_chien_muoi_xa
echo "Brochettes de poisson grillees" >> $LFS_ROOT/main/monkfish/nuoc_mam/safran/gingembre/ciboule/Cha_ca_muong
echo "Poisson au the" >> $LFS_ROOT/main/tuna/the/gingembre/nuoc_mam/pork/Ca_kho_tra
echo "Boulettes de langoustines frites" >> $LFS_ROOT/main/shrimp/oeuf/nuoc_mam/glutamate/gingembre/Cha_tom
echo "Crabe a la sauce aigre-douce" >> $LFS_ROOT/main/crab/sauce_soja/nuoc_mam/coriandre/Cua_xao_chua_ngot
echo "Abalones sautes aux mushrooms parfumes" >> $LFS_ROOT/main/abalone/volaille/mushroom/glutamate/sauce_soja/ciboule/Bao_ngu_xao_dau_hao
echo "Seiches sautées au céleri" >> $LFS_ROOT/main/cuttlefish/celeri/glutamate/coriandre/Muc_xao_can
echo "Boulettes de crevettes grillees" >> $LFS_ROOT/main/shrimp/oeuf/gingembre/mint/rice/nuoc_mam/Chao_tom
echo "Langoustines a la sauce piquante" >> $LFS_ROOT/main/shrimp/gingembre/tomato/glutamate/Tom_sot_cay
echo "Crabes au sel" >> $LFS_ROOT/main/crab/Cua_rang_muoi
echo "Poisson en sauce" >> $LFS_ROOT/main/monkfish/safran/mam_tom/ciboule/mint/Ca_kho
echo "Pate de seiche" >> $LFS_ROOT/main/cuttlefish/nuoc_mam/glutamate/oeuf/Cha_muc

# main: beef
echo "Boeuf grillé en crépine" >> $LFS_ROOT/main/beef/ciboule/glutamate/Bo_boc_mo_chai
echo "Nouilles garnies à la Hanoienne" >> $LFS_ROOT/main/beef/shrimp/chou_vert/pate_riz/glutamate/Pho_xao
echo "Lamelles de boeuf en brochettes" >> $LFS_ROOT/main/beef/glutamate/mint/coriandre/concombre/rice/nuoc_mam/Bo_lui
echo "Sauté de boeuf au soja" >> $LFS_ROOT/main/beef/sauce_soja/glutamate/Bo_xao_gia
echo "Boeuf grillé sur plaque" >> $LFS_ROOT/main/beef/citronnelle/glutamate/rice/mint/coriandre/concombre/nuoc_mam/Bo_nuong_luoi_cuoc
echo "Boeuf en sauce" >> $LFS_ROOT/main/beef/gingembre/soya_sale/citronnelle/vermicelle/Bo_kho
echo "Boeuf sauté à la sauce d'huitre" >> $LFS_ROOT/main/beef/gingembre/citronnelle/sauce_huitre/ciboule/Bo_xao_dau_hao

# main: pork
echo "Jambonneau à l'étouffée" >> $LFS_ROOT/main/pork/pousse_bambou/nuoc_mam/vermicelle/glutamate/ciboule/Gio_heo_ham_mang
echo "Travers de porc à la citronnelle" >> $LFS_ROOT/main/pork/citronnelle/5parfums/Suon_muong
echo "Fromage de tête de porc" >> $LFS_ROOT/main/pork/nuoc_mam/mushroom/glutamate/Gio_thu
echo "Fromage de soja à la vapeur" >> $LFS_ROOT/main/pork/soya_cheese/ciboule/pate_soja/mushroom/chou_sale/Tau_hu_hap
echo "Grillade de porc" >> $LFS_ROOT/main/pork/nuoc_mam/glutamate/Heo_lui
echo "Porc au caramel" >> $LFS_ROOT/main/pork/nuoc_mam/coconut/Heo_kho
echo "Ragoût de pieds de porc en faux gibier" >> $LFS_ROOT/main/pork/safran/mam_tom/coriandre/Gia_cay
echo "Patés de viande frits" >> $LFS_ROOT/main/pork/rice_alcohol/nuoc_mam/Cha_chien
echo "Paté de maïs" >> $LFS_ROOT/main/pork/corn/nuoc_mam/glutamate/galette_riz/salade/mint/Cha_bap
echo "Paté de porc aux mushrooms" >> $LFS_ROOT/main/pork/mushroom/petit_pois/oeuf/vermicelle/nuoc_mam/glutamate/Nam_nuong
echo "Aspic de porc" >> $LFS_ROOT/main/pork/nuoc_mam/Thit_dong

# main: gibier
echo "Grillade de chevreuil" >> $LFS_ROOT/main/gibier/citronnelle/nuoc_mam/5parfums/soya_sale/Nai_nuong
echo "Chevreuil sauté paysanne" >> $LFS_ROOT/main/gibier/soya_sale/citronnelle/coconut/nuoc_mam/glutamate/Nai_xao_lang
echo "Chevreau rôti" >> $LFS_ROOT/main/gibier/rice_alcohol/pate_soja/glutamate/5parfums/De_nuong_chao

# main: poultry
echo "Coquelet aux cinq parfums" >> $LFS_ROOT/main/poultry/rice_alcohol/sauce_soja/5parfums/Ga_nuong_ngu_vi
echo "Coquelet au corn à la vapeur" >> $LFS_ROOT/main/poultry/corn/mushroom/ciboule/sauce_soja/Ga_hap_ngo
echo "Poulet au riz" >> $LFS_ROOT/main/poultry/rice/carotte/citronnelle/sauce_soja/glutamate/Com_ga
echo "Poulet grillé au soja" >> $LFS_ROOT/main/poultry/soya_cheese/soya_sale/5parfums/Ga_nuong_chao
echo "Poulet au gingembre" >> $LFS_ROOT/main/poultry/gingembre/soya_sale/Ga_xao_gung
echo "Poulet sauté" >> $LFS_ROOT/main/poultry/nuoc_mam/Ga_ram
echo "Poulet désossé farci" >> $LFS_ROOT/main/poultry/pork/mushroom/vermicelle/glutamate/rice_alcohol/Ga_rut_xuong
echo "Brochettes de poulet" >> $LFS_ROOT/main/poultry/pork/rice_alcohol/5parfums/sauce_soja/soya_sale/salade/Ga_lui
echo "Abattis de volaille au bain-marie" >> $LFS_ROOT/main/poultry/oeuf/coconut/nuoc_mam/vermicelle/mushroom/glutamate/Long_ga_hap
echo "Poule au curry" >> $LFS_ROOT/main/poultry/coconut/tomato/glutamate/curry/Ga_ca_ri
echo "Abattis de volaille sautés" >> $LFS_ROOT/main/poultry/cu_nang/mushroom/ciboule/nuoc_mam/sauce_soja/ciboule/coriandre/Long_ga_xao
echo "Pigeonneau laqué" >> $LFS_ROOT/main/poultry/rice_alcohol/sauce_soja/glutamate/5parfums/carmin/Bo_cau_quay
echo "Canard à la vapeur" >> $LFS_ROOT/main/poultry/lotus/pruneau/mushroom/gingembre/rice_alcohol/glutamate/nuoc_mam/Vit_tim

# les fondues
echo "Fondue de boeuf" >> $LFS_ROOT/fondue/beef/salade/mint/coriandre/persicaire/concombre/galette_riz/celeri/nuoc_mam/Bo_nhung_dam
echo "Fondue variée" >> $LFS_ROOT/fondue/beef/monkfish/cuttlefish/volaille/shrimp/coriandre/vermicelle/soya_bean_sprout/salade/glutamate/ciboule/Thap_cam_nhung_dam

# main: rice
echo "Riz blanc" >> $LFS_ROOT/main/rice/Com
echo "Riz sauté" >> $LFS_ROOT/main/rice/oeuf/shrimp/saucisse_chinoise/Com_rang
echo "Riz en miche" >> $LFS_ROOT/main/rice/Com_nam
echo "" > $LFS_ROOT/main/rice/glutinous_rice/epinard/haricot_vert/pork/Banh_chung_xanh


# vegetarian meals
# cd vegetarien
# echo "Aubergines sautées" >> gingembre/glutamate/sauce_soja/Ca_tim_xao
# cd ..

# desserts
echo "Beignets" >> $LFS_ROOT/dessert/apple/banana/ananas/Chuoi_chien
echo "Fromage de soja cornon" >> $LFS_ROOT/dessert/grain_soja/agar_agar/gingembre/Tau_hu
echo "Gateau aux amandes" >> $LFS_ROOT/dessert/oeuf/vanille/Banh_hanh_nhan
echo "Gateau au tapioca" >> $LFS_ROOT/dessert/tapioca/soya_vert/coconut/Banh_ech_tran
echo "Flan à la vietnamienne" >> $LFS_ROOT/dessert/oeuf/coconut/vanille/Banh_gan
echo "Noix de coconut surprise" >> $LFS_ROOT/dessert/oeuf/coconut/vanille/Kem_dua
echo "Riz gluant à la noix de coconut" >> $LFS_ROOT/dessert/haricot_blanc/glutinous_rice/coconut/vanille/Xoi_dua
echo "bananas aux noix de coconut" >> $LFS_ROOT/dessert/banana/coconut/tapioca/vanille/Chuoi_xao_dua
echo "Gateau de bananas au four" >> $LFS_ROOT/dessert/banana/coconut/vanille/Banh_chuoi_nuong
echo "Bananas au caramel" >> $LFS_ROOT/dessert/banana/soya_alcohol/Chuoi_duong_thang
echo "Caramel de bananas" >> $LFS_ROOT/dessert/banana/coconut/vanille/Keo_chuoi
echo "Gateau de fleurs d'orchidées" >> $LFS_ROOT/dessert/oeuf/vanille/Banh_bong_lan
echo "Salade de fruits exotiques" >> $LFS_ROOT/dessert/lychee/longane/ananas/banana/orange/Hoa_qua_tron

# drinks
echo "Le thé" >> $LFS_ROOT/the/Tra
echo "Lait de soja" >> $LFS_ROOT/drink/grain_soja/Sua_dau_nanh

# ordinary menues
mv $LFS_ROOT/.ext/Goi_gia           $LFS_ROOT/menu1
mv $LFS_ROOT/.ext/Muc_xao_can       $LFS_ROOT/menu1
mv $LFS_ROOT/.ext/Suon_muong        $LFS_ROOT/menu1
mv $LFS_ROOT/.ext/Ca_kho            $LFS_ROOT/menu1
mv $LFS_ROOT/.ext/Com               $LFS_ROOT/menu1
mv $LFS_ROOT/.ext/Chuoi_duong_thang $LFS_ROOT/menu1

mv $LFS_ROOT/.ext/Chao_ga         $LFS_ROOT/menu2
mv $LFS_ROOT/.ext/Cha_cua_hay_tom $LFS_ROOT/menu2
mv $LFS_ROOT/.ext/Bo_kho          $LFS_ROOT/menu2
mv $LFS_ROOT/.ext/Com             $LFS_ROOT/menu2
mv $LFS_ROOT/.ext/Banh_hanh_nhan  $LFS_ROOT/menu2

mv $LFS_ROOT/.ext/Nom_sua        $LFS_ROOT/menu3
mv $LFS_ROOT/.ext/Bo_xao_dau_hao $LFS_ROOT/menu3
mv $LFS_ROOT/.ext/Heo_kho        $LFS_ROOT/menu3
mv $LFS_ROOT/.ext/Ca_nhoi        $LFS_ROOT/menu3
mv $LFS_ROOT/.ext/Com            $LFS_ROOT/menu3
mv $LFS_ROOT/.ext/Banh_gan       $LFS_ROOT/menu3

mv $LFS_ROOT/.ext/Chao_bot_ban $LFS_ROOT/menu4
mv $LFS_ROOT/.ext/Tau_hu_hap   $LFS_ROOT/menu4
mv $LFS_ROOT/.ext/Ca_nau_nung  $LFS_ROOT/menu4
mv $LFS_ROOT/.ext/Com          $LFS_ROOT/menu4

# Party menues
mv $LFS_ROOT/.ext/Bo_nuong_luoi_cuoc  $LFS_ROOT/menu_I
mv $LFS_ROOT/.ext/Nai_xao_lang        $LFS_ROOT/menu_I
mv $LFS_ROOT/.ext/Bao_ngu_xao_dau_hao $LFS_ROOT/menu_I
mv $LFS_ROOT/.ext/Com                 $LFS_ROOT/menu_I
mv $LFS_ROOT/.ext/Hoa_qua_tron        $LFS_ROOT/menu_I

mv $LFS_ROOT/.ext/Mang_cua          $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Cha_tom           $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Nom_sua           $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Bo_boc_mo_chai    $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Luong_um          $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Com               $LFS_ROOT/menu_II
mv $LFS_ROOT/.ext/Chuoi_duong_thang $LFS_ROOT/menu_II

mv $LFS_ROOT/.ext/De_nuong_chao     $LFS_ROOT/menu_III
mv $LFS_ROOT/.ext/Cua_xao_chua_ngot $LFS_ROOT/menu_III
mv $LFS_ROOT/.ext/Chao_bao_ngu      $LFS_ROOT/menu_III
mv $LFS_ROOT/.ext/Banh_cuon         $LFS_ROOT/menu_III

# les plats complets
mv $LFS_ROOT/.ext/Com_ga  $LFS_ROOT/plat-complet/Com_ga
mv $LFS_ROOT/.ext/Pho_xao $LFS_ROOT/plat-complet/Pho_xao
mv $LFS_ROOT/.ext/Vit_tim $LFS_ROOT/plat-complet/Vit_tim


