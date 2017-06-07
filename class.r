cls_av = list()
cls_av[["Fighter"]] = list(class_name="Fighter",
                           hit_die=10,
                           BAB="high",
                           fort="high",
                           reflex="low",
                           will="low",
                           skill_points=2,
                           prereq_BAB=0,
                           prereq_feats=c(),
                           prereq_skills=list(),
                           class_skills=c(),
                           feats= vector("list", 30))
cls_av[["Monk"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                      "Weapon Proficiency (Simple)")


cls_av[["Monk"]] = list(class_name="Monk",
                        hit_die=8,
                        BAB="medium",
                        fort="high",
                        reflex="high",
                        will="high",
                        skill_points=4,
                        prereq_BAB=0,
                        prereq_feats=c(),
                        prereq_skills=list(),
                        class_skills=c(),
                        feats= vector("list", 30))
cls_av[["Monk"]][["feats"]][[1 ]] = c("Flurry of Blows", 
                                      "Unarmed Strike", 
                                      "Stunning Fist",
                                      "Weapon Proficiency (Monk)")
cls_av[["Monk"]][["feats"]][[4 ]] = c("Ki Strike (Magic)")
cls_av[["Monk"]][["feats"]][[11]] = c("Greater Flurry")
cls_av[["Monk"]][["feats"]][[16]] = c("Ki Strike (Adamantine)")

cls_av[["Swashbuckler"]] = list(class_name="Swashbuckler",
                                hit_die=10,
                                BAB="high",
                                fort="high",
                                reflex="high",
                                will="low",
                                skill_points=4,
                                prereq_BAB=0,
                                prereq_feats=c(),
                                prereq_skills=list(),
                                class_skills=c("bluff"),
                                feats= vector("list", 30))
cls_av[["Swashbuckler"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                              "Weapon Proficiency (Simple)",
                                              "Weapon Finesse")
cls_av[["Swashbuckler"]][["feats"]][[5 ]] = c("Insightful Strike")
cls_av[["Swashbuckler"]][["feats"]][[7 ]] = c("Mobility")
cls_av[["Swashbuckler"]][["feats"]][[14]] = c("Weakening Critical")
cls_av[["Swashbuckler"]][["feats"]][[19]] = c("Wounding Critical")

cls_av[["Dervish"]] = list(class_name="Dervish",
                           hit_die=10,
                           BAB="high",
                           fort="low",
                           reflex="high",
                           will="high",
                           skill_points=4,
                           prereq_BAB=5,
                           prereq_feats=c("Combat Expertise",
                                          "Dodge",
                                          "Weapon Focus (Slashing)"),
                           prereq_skills=list("perform"=3,
                                              "tumble"=3),
                           class_skills=c("bluff"),
                           feats= vector("list", 10))
cls_av[["Dervish"]][["feats"]][[1 ]] = c("Dervish Dance")
cls_av[["Dervish"]][["feats"]][[3 ]] = c("Spring Attack")
cls_av[["Dervish"]][["feats"]][[6 ]] = c("Improved Initiative")
cls_av[["Dervish"]][["feats"]][[9 ]] = c("Tireless Dance")
cls_av[["Dervish"]][["feats"]][[10]] = c("A Thousand Cuts")

cls_av[["Invisible Blade"]] = list(class_name="Invisible Blade",
                                   hit_die=6,
                                   BAB="high",
                                   fort="low",
                                   reflex="high",
                                   will="low",
                                   skill_points=4,
                                   prereq_BAB=0,
                                   prereq_feats=c("Feint",
                                                  "Two-Weapon Fighting",
                                                  "Weapon Focus (Invisible Blade)"),
                                   prereq_skills=list("bluff"=8),
                                   class_skills=c("bluff"),
                                   feats= vector("list", 5))
cls_av[["Invisible Blade"]][["feats"]][[1 ]] = c("Bleeding Wound I")
cls_av[["Invisible Blade"]][["feats"]][[3 ]] = c("Bleeding Wound II")
cls_av[["Invisible Blade"]][["feats"]][[5 ]] = c("Bleeding Wound III",
                                                 "Feint Mastery")