cls_av = list()
cls_av[["Fighter"]] = list(class_name="Fighter",
                           base=T,
                           hit_die=10,
                           BAB="high",
                           fortitude="high",
                           reflex="low",
                           will="low",
                           skill_points=2,
                           prereq_BAB=0,
                           prereq_feats=c(),
                           prereq_skills=list(),
                           class_skills=c("Craft Armor", "Craft Weapon", "Intimidate", "Parry", "Taunt", "Tumble"),
                           feats= vector("list", 30),
                           bonus_feats=c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))
cls_av[["Fighter"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                         "Weapon Proficiency (Simple)",
                                         "Armor Proficiency (Light)",
                                         "Armor Proficiency (Medium)",
                                         "Armor Proficiency (Heavy)")


cls_av[["Monk"]] = list(class_name="Monk",
                        base=T,
                        hit_die=8,
                        BAB="medium",
                        fortitude="high",
                        reflex="high",
                        will="high",
                        skill_points=4,
                        prereq_BAB=0,
                        prereq_feats=c(),
                        prereq_skills=list("Concentration", "Craft Alchemy", "Craft Trap", "Diplomacy", "Hide", "Listen", "Move Silently", "Parry", "Spot", "Tumble"),
                        class_skills=c(),
                        feats= vector("list", 30))
cls_av[["Monk"]][["feats"]][[1 ]] = c("Flurry of Blows", 
                                      "Unarmed Strike", 
                                      "Stunning Fist",
                                      "Weapon Proficiency (Monk)",
                                      "Monk AC Bonus")
cls_av[["Monk"]][["feats"]][[4 ]] = c("Ki Strike (Magic)")
cls_av[["Monk"]][["feats"]][[5 ]] = c("Monk AC I")
cls_av[["Monk"]][["feats"]][[10]] = c("Monk AC II")
cls_av[["Monk"]][["feats"]][[11]] = c("Greater Flurry")
cls_av[["Monk"]][["feats"]][[15]] = c("Monk AC III")
cls_av[["Monk"]][["feats"]][[16]] = c("Ki Strike (Adamantine)")
cls_av[["Monk"]][["feats"]][[20]] = c("Monk AC IV")
cls_av[["Monk"]][["feats"]][[25]] = c("Monk AC V")
cls_av[["Monk"]][["feats"]][[30]] = c("Monk AC VI")

cls_av[["Swashbuckler"]] = list(class_name="Swashbuckler",
                                base=T,
                                hit_die=10,
                                BAB="high",
                                fortitude="high",
                                reflex="high",
                                will="low",
                                skill_points=4,
                                prereq_BAB=0,
                                prereq_feats=c(),
                                prereq_skills=list(),
                                class_skills=c("Bluff", "Craft Armor", "Craft Weapon", "Diplomacy", "Intimidate", "Parry", "Taunt", "Tumble"),
                                feats= vector("list", 30))
cls_av[["Swashbuckler"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                              "Weapon Proficiency (Simple)",
                                              "Weapon Finesse",
                                              "Armor Proficiency (Light)")
cls_av[["Swashbuckler"]][["feats"]][[5 ]] = c("Insightful Strike",
                                              "Swashbuckler Dodge I")
cls_av[["Swashbuckler"]][["feats"]][[7 ]] = c("Mobility")
cls_av[["Swashbuckler"]][["feats"]][[10]] = c("Swashbuckler Dodge II")
cls_av[["Swashbuckler"]][["feats"]][[14]] = c("Weakening Critical")
cls_av[["Swashbuckler"]][["feats"]][[15]] = c("Swashbuckler Dodge III")
cls_av[["Swashbuckler"]][["feats"]][[19]] = c("Wounding Critical")
cls_av[["Swashbuckler"]][["feats"]][[20]] = c("Swashbuckler Dodge IV")
cls_av[["Swashbuckler"]][["feats"]][[25]] = c("Swashbuckler Dodge V")
cls_av[["Swashbuckler"]][["feats"]][[30]] = c("Swashbuckler Dodge VI")

cls_av[["Dervish"]] = list(class_name="Dervish",
                           base=F,
                           hit_die=10,
                           BAB="high",
                           fortitude="low",
                           reflex="high",
                           will="high",
                           skill_points=4,
                           prereq_BAB=5,
                           prereq_feats=c("Combat Expertise",
                                          "Dodge",
                                          "Weapon Focus (Slashing)"),
                           prereq_skills=list("Perform"=3,
                                              "Tumble"=3),
                           class_skills=c("Bluff", "Perform", "Craft Weapon", "Craft Armor", "Listen", "Parry", "Perform", "Taunt", "Tumble"),
                           feats= vector("list", 10))
cls_av[["Dervish"]][["feats"]][[1 ]] = c("Dervish Dance",
                                         "Dervish AC Bonus I")
cls_av[["Dervish"]][["feats"]][[3 ]] = c("Spring Attack")
cls_av[["Dervish"]][["feats"]][[5 ]] = c("Dervish AC Bonus II")
cls_av[["Dervish"]][["feats"]][[6 ]] = c("Improved Initiative")
cls_av[["Dervish"]][["feats"]][[9 ]] = c("Tireless Dance")
cls_av[["Dervish"]][["feats"]][[10]] = c("A Thousand Cuts",
                                         "Dervish AC Bonus III")

cls_av[["Invisible Blade"]] = list(class_name="Invisible Blade",
                                   base=F,
                                   hit_die=6,
                                   BAB="high",
                                   fortitude="low",
                                   reflex="high",
                                   will="low",
                                   skill_points=4,
                                   prereq_BAB=0,
                                   prereq_feats=c("Feint",
                                                  "Two-Weapon Fighting",
                                                  "Weapon Focus (Invisible Blade)"),
                                   prereq_skills=list("Bluff"=8),
                                   class_skills=c("Bluff"),
                                   feats= vector("list", 5))
cls_av[["Invisible Blade"]][["feats"]][[1 ]] = c("Bleeding Wound I",
                                                 "Unfettered Defense")
cls_av[["Invisible Blade"]][["feats"]][[3 ]] = c("Bleeding Wound II")
cls_av[["Invisible Blade"]][["feats"]][[5 ]] = c("Bleeding Wound III",
                                                 "Feint Mastery")

cls_av[["Tempest"]] = list(class_name="Tempest",
                           base=F,
                           hit_die=10,
                           BAB="high",
                           fortitude="high",
                           reflex="low",
                           will="low",
                           skill_points=2,
                           prereq_BAB=6,
                           prereq_feats=c("Dodge", "Mobility", "Spring Attack", "Improved Two-Weapon Fighting"),
                           prereq_skills=list(),
                           class_skills=c(),
                           feats= vector("list", 30))
cls_av[["Tempest"]][["feats"]][[1 ]] = c("Tempest Defense I")
cls_av[["Tempest"]][["feats"]][[1 ]] = c("Tempest Defense II")
cls_av[["Tempest"]][["feats"]][[1 ]] = c("Tempest Defense III", "Greater Two-Weapon Fighting")
cls_av[["Tempest"]][["feats"]][[1 ]] = c("Tempest Defense IV")
cls_av[["Tempest"]][["feats"]][[1 ]] = c("Tempest Defense V", "Whirlwind Attack", "Tempest Whirlwind")

cls_av[["Ranger"]] = list(class_name="Ranger",
                          base=T,
                          hit_die=8,
                          isCaster=T,
                          isDivineCaster=T,
                          BAB="high",
                          fortitude="high",
                          reflex="high",
                          will="low",
                          skill_points=6,
                          prereq_BAB=0,
                          prereq_feats=c(),
                          prereq_skills=list(),
                          class_skills=c(),
                          feats= vector("list", 30),
                          bonus_feats=c(2,2,6,11,21),
                          spells=vector("list", 30),
                          spell_stat="Wisdom",
                          spell_build=list())
cls_av[["Ranger"]][["spells"]][[4 ]] = c(0, 0)
cls_av[["Ranger"]][["spells"]][[5 ]] = c(0, 0)
cls_av[["Ranger"]][["spells"]][[6 ]] = c(0, 1)
cls_av[["Ranger"]][["spells"]][[7 ]] = c(0, 1)
cls_av[["Ranger"]][["spells"]][[8 ]] = c(0, 1, 0)
cls_av[["Ranger"]][["spells"]][[9 ]] = c(0, 1, 0)
cls_av[["Ranger"]][["spells"]][[10]] = c(0, 1, 1)
cls_av[["Ranger"]][["spells"]][[11]] = c(0, 1, 1, 0)
cls_av[["Ranger"]][["spells"]][[12]] = c(0, 1, 1, 1)
cls_av[["Ranger"]][["spells"]][[13]] = c(0, 1, 1, 1)
cls_av[["Ranger"]][["spells"]][[14]] = c(0, 2, 1, 1, 0)
cls_av[["Ranger"]][["spells"]][[15]] = c(0, 2, 1, 1, 1)
cls_av[["Ranger"]][["spells"]][[16]] = c(0, 2, 2, 1, 1)
cls_av[["Ranger"]][["spells"]][[17]] = c(0, 2, 2, 2, 1)
cls_av[["Ranger"]][["spells"]][[18]] = c(0, 3, 2, 2, 1)
cls_av[["Ranger"]][["spells"]][[19]] = c(0, 3, 3, 3, 2)
cls_av[["Ranger"]][["spells"]][[20]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[21]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[22]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[23]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[24]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[25]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[26]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[27]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[28]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[29]] = c(0, 3, 3, 3, 3)
cls_av[["Ranger"]][["spells"]][[30]] = c(0, 3, 3, 3, 3)

cls_av[["Ranger"]][["spell_list"]] = list()
cls_av[["Ranger"]][["spell_list"]][[1 ]] = c()
cls_av[["Ranger"]][["spell_list"]][[2 ]] = c("Blades of Fire")
cls_av[["Ranger"]][["spell_list"]][[3 ]] = c("Cat's Grace", "Animalistic Power", 
                                             `Extend Spell`="Blades of Fire")
cls_av[["Ranger"]][["spell_list"]][[4 ]] = c(`Extend Spell`="Cat's Grace", `Extend Spell`="Animalistic Power")
cls_av[["Ranger"]][["spell_list"]][[5 ]] = c()

# cls_av[["Ranger"]][["spell_build"]][["Offensive Melee"]] = list()
# cls_av[["Ranger"]][["spell_build"]][["Offensive Melee"]][[2 ]] = c("Blades of Fire", "Blades of Fire")
# cls_av[["Ranger"]][["spell_build"]][["Offensive Melee"]][[3 ]] = c("Cat's Grace", "Animalistic Power", "Cat's Grace", "Animalistic Power")


cls_av[["Ranger"]][["feats"]][[1 ]] = c("Track",
                                        "Weapon Proficiency (Martial)",
                                        "Weapon Proficiency (Simple)",
                                        "Ranger Spellcasting",
                                        "Favored Enemy I",
                                        "Armor Proficiency (Light)")
cls_av[["Ranger"]][["feats"]][[3 ]] = c("Toughness")
cls_av[["Ranger"]][["feats"]][[5 ]] = c("Favored Enemy II")
cls_av[["Ranger"]][["feats"]][[9 ]] = c("Evasion")
cls_av[["Ranger"]][["feats"]][[10]] = c("Favored Enemy III")
cls_av[["Ranger"]][["feats"]][[15]] = c("Favored Enemy IV")
cls_av[["Ranger"]][["feats"]][[20]] = c("Favored Enemy V")
cls_av[["Ranger"]][["feats"]][[25]] = c("Favored Enemy VI")
cls_av[["Ranger"]][["feats"]][[30]] = c("Favored Enemy VII")

cls_av[["Whirling Dervish"]] = list(class_name="Whirling Dervish",
                          base=F,
                          hit_die=6,
                          BAB="medium",
                          fortitude="low",
                          reflex="high",
                          will="low",
                          skill_points=6,
                          prereq_BAB=5,
                          prereq_feats=c("Dodge", "Mobility"),
                          prereq_skills=list("Hide"=8,
                                             "Move Silently"=8,
                                             "Tumble"=8),
                          class_skills=c("Appraise", "Bluff", "Concentration", "Craft Armor", "Craft Trap", "Craft Weapon", "Disable Device",
                                         "Hide", "Move Silently", "Open Locks", "Perform", "Search", "Set Trap", "Spot", "Tumble", "Use Magical Device"),
                          feats= vector("list", 30),
                          bonus_feats=c(2,2,2,7,7,7))
cls_av[["Whirling Dervish"]][["feats"]][[1 ]] = c("Dash")
cls_av[["Whirling Dervish"]][["feats"]][[2 ]] = c("Evasion")
cls_av[["Whirling Dervish"]][["feats"]][[3 ]] = c("Spring Attack", "Sneak Attack +1d6")
cls_av[["Whirling Dervish"]][["feats"]][[4 ]] = c("Critical Sense 1")
cls_av[["Whirling Dervish"]][["feats"]][[5 ]] = c("Expert Tumbling")
cls_av[["Whirling Dervish"]][["feats"]][[6 ]] = c("Whirlwind Attack", "Sneak Attack +2d6")
cls_av[["Whirling Dervish"]][["feats"]][[8 ]] = c("Defensive Roll")
cls_av[["Whirling Dervish"]][["feats"]][[9 ]] = c("Sneak Attack +3d6")
cls_av[["Whirling Dervish"]][["feats"]][[10 ]] = c("Critical Sense 2", "Minor Teleportation")


cls_av[["Wizard"]] = list(class_name="Wizard",
                          base=T,
                          hit_die=4,
                          isCaster=T,
                          isArcaneCaster=T,
                          BAB="low",
                          fortitude="low",
                          reflex="low",
                          will="high",
                          skill_points=2,
                          prereq_BAB=0,
                          prereq_feats=c(),
                          prereq_skills=list(),
                          class_skills=c("Concentration", "Spellcraft"),
                          feats= vector("list", 30),
                          bonus_feats=c(5,10,15,20,23,26,29),
                          spells=vector("list", 30),
                          spell_list=vector("list", 10),
                          spell_stat="Intelligence",
                          spell_DC_stat="Intelligence",
                          spell_build=list())
cls_av[["Wizard"]][["spells"]][[1 ]] = c(3, 1)
cls_av[["Wizard"]][["spells"]][[2 ]] = c(4, 2)
cls_av[["Wizard"]][["spells"]][[3 ]] = c(4, 2, 1)
cls_av[["Wizard"]][["spells"]][[4 ]] = c(4, 3, 2)
cls_av[["Wizard"]][["spells"]][[5 ]] = c(4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[6 ]] = c(4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[7 ]] = c(4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[8 ]] = c(4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[9 ]] = c(4, 4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[10]] = c(4, 4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[11]] = c(4, 4, 4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[12]] = c(4, 4, 4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[13]] = c(4, 4, 4, 4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[14]] = c(4, 4, 4, 4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[15]] = c(4, 4, 4, 4, 4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[16]] = c(4, 4, 4, 4, 4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[17]] = c(4, 4, 4, 4, 4, 4, 4, 3, 2, 1)
cls_av[["Wizard"]][["spells"]][[18]] = c(4, 4, 4, 4, 4, 4, 4, 3, 3, 2)
cls_av[["Wizard"]][["spells"]][[19]] = c(4, 4, 4, 4, 4, 4, 4, 4, 3, 3)
cls_av[["Wizard"]][["spells"]][[20]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[21]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[22]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[23]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[24]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[25]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[26]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[27]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[28]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[29]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
cls_av[["Wizard"]][["spells"]][[30]] = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4)



cls_av[["Wizard"]][["spell_list"]][[1 ]] = c("Ray of Frost", "Acid Splash")
cls_av[["Wizard"]][["spell_list"]][[2 ]] = c("Magic Missile")
cls_av[["Wizard"]][["spell_list"]][[3 ]] = c("Melf's Acid Arrow", "Gedlee's Electric Loop", "Mirror Image")
cls_av[["Wizard"]][["spell_list"]][[4 ]] = c("Flame Arrow", "Fireball", "Displacement")
cls_av[["Wizard"]][["spell_list"]][[5 ]] = c("Isaac's Lesser Missile Storm", "Ice Storm")
cls_av[["Wizard"]][["spell_list"]][[6 ]] = c("Arc of Lightning")
cls_av[["Wizard"]][["spell_list"]][[7 ]] = c("Isaac's Greater Missile Storm", "Chain Lightning")
cls_av[["Wizard"]][["spell_list"]][[8 ]] = c("Delayed Blast Fireball")
cls_av[["Wizard"]][["spell_list"]][[9 ]] = c("Polar Ray", "Horrid Wilting")
cls_av[["Wizard"]][["spell_list"]][[10]] = c("Burst of Glacial Wrath")

cls_av[["Wizard"]][["feats"]][[1]] = c("Scribe Scroll",
                                        "Summon Familiar",
                                        "Weapon Proficiency (Wizard)",
                                        "Wizard Spellcasting")


cls_av[["Arcane Scholar"]] = list(class_name="Arcane Scholar",
                                    base=F,
                                    hit_die=4,
                                    BAB="low",
                                    fortitude="low",
                                    reflex="low",
                                    will="high",
                                    skill_points=2,
                                    prereq_feats=c("Empower Spell", "Skill Focus (Concentration)", "Skill Focus (Spellcraft)"),
                                    prereq_skills=list("Spellcraft"=8),
                                    prereq_arcane_spellLevel=3,
                                    class_skills=c("Appraise", "Concentration", "Craft Alchemy", "Diplomacy", "Search", "Spellcraft"),
                                    feats= vector("list", 30),
                                    bonus_feats=c(1))
cls_av[["Arcane Scholar"]][["feats"]][[1 ]] = c("Maximize Spell")
cls_av[["Arcane Scholar"]][["feats"]][[2 ]] = c("Spell Knowledge +1")
cls_av[["Arcane Scholar"]][["feats"]][[3 ]] = c("Improved Empower Spell")
cls_av[["Arcane Scholar"]][["feats"]][[4 ]] = c()
cls_av[["Arcane Scholar"]][["feats"]][[5 ]] = c("Quicken Spell")
cls_av[["Arcane Scholar"]][["feats"]][[6 ]] = c()
cls_av[["Arcane Scholar"]][["feats"]][[7 ]] = c("Improved Maximize Spell")
cls_av[["Arcane Scholar"]][["feats"]][[8 ]] = c("Spell Knowledge +2")
cls_av[["Arcane Scholar"]][["feats"]][[9 ]] = c()
cls_av[["Arcane Scholar"]][["feats"]][[10 ]] = c("Improved Quicken Spell")


setupClass_metamagics = function(class_avail, spells_avail, feats_avail){
  class_avail = cls_av
  spells_avail = spells_av
  feats_avail = feats_av
  for(class_n in names(class_avail)){
    if(is.null(class_avail[[class_n]]$isCaster)) next
    if(!class_avail[[class_n]]$isCaster) next
    if(is.null(class_avail[[class_n]]$spell_list)) next
    if(length(class_avail[[class_n]]$spell_list) == 0) next
    
    for( spell_level in 1:length(class_avail[[class_n]]$spell_list) ){
      spell_list = class_avail[[class_n]]$spell_list[[spell_level]]
      if(is.null(spell_list)) next
      
      if(is.null(names(spell_list))) names(class_avail[[class_n]]$spell_list[[spell_level]]) = rep("None", length(spell_list))
      if(!is.null(names(spell_list))) names(class_avail[[class_n]]$spell_list[[spell_level]])[is.na(names(spell_list))] = "None"
      spell_list = class_avail[[class_n]]$spell_list[[spell_level]]
      spell_list = spell_list[names(spell_list) == "None"]
      
      for( spell_n in spell_list ){
        if(is.null(spells_avail[[spell_n]]$metamagics)) next
        for( metamagic in spells_avail[[spell_n]]$metamagics ){
          if(is.null(feats_avail[[metamagic]]$levelAdjust)) break
          mm_sl = spell_level + feats_avail[[metamagic]]$levelAdjust
          if( mm_sl > length(class_avail[[class_n]]$spell_list) ) next
          
          sl_spellList = class_avail[[class_n]]$spell_list[[mm_sl]]
          
          class_avail[[class_n]]$spell_list[[mm_sl]] = c(sl_spellList, spell_n)
          names(class_avail[[class_n]]$spell_list[[mm_sl]])[length(sl_spellList)+1] = metamagic
        }
      }
      sl_spellList = class_avail[[class_n]]$spell_list[[spell_level]]
      class_avail[[class_n]]$spell_list[[spell_level]] = sl_spellList[!duplicated(cbind(names(sl_spellList), sl_spellList))]
    }
  }
  class_avail$Wizard$spell_list
  return(class_avail)
}
cls_av = setupClass_metamagics(cls_av, spells_av, feats_av)
