classes_available = list()
classes_available[["fighter"]] = list(class_name="Fighter",
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
classes_available[["monk"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                                 "Weapon Proficiency (Simple)")


classes_available[["monk"]] = list(class_name="Monk",
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
classes_available[["monk"]][["feats"]][[1 ]] = c("Flurry of Blows", 
                                                 "Unarmed Strike", 
                                                 "Stunning Fist",
                                                 "Weapon Proficiency (Monk)")
classes_available[["monk"]][["feats"]][[4 ]] = c("Ki Strike (Magic)")
classes_available[["monk"]][["feats"]][[11]] = c("Greater Flurry")
classes_available[["monk"]][["feats"]][[16]] = c("Ki Strike (Adamantine)")

classes_available[["swashbuckler"]] = list(class_name="Swashbuckler",
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
classes_available[["swashbuckler"]][["feats"]][[1 ]] = c("Weapon Proficiency (Martial)", 
                                                         "Weapon Proficiency (Simple)",
                                                         "Weapon Finesse")
classes_available[["swashbuckler"]][["feats"]][[5 ]] = c("Insightful Strike")
classes_available[["swashbuckler"]][["feats"]][[7 ]] = c("Mobility")
classes_available[["swashbuckler"]][["feats"]][[14]] = c("Weakening Critical")
classes_available[["swashbuckler"]][["feats"]][[19]] = c("Wounding Critical")

classes_available[["dervish"]] = list(class_name="Dervish",
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
classes_available[["dervish"]][["feats"]][[1 ]] = c("Dervish Dance")
classes_available[["dervish"]][["feats"]][[3 ]] = c("Spring Attack")
classes_available[["dervish"]][["feats"]][[6 ]] = c("Improved Initiative")
classes_available[["dervish"]][["feats"]][[9 ]] = c("Tireless Dance")
classes_available[["dervish"]][["feats"]][[10]] = c("A Thousand Cuts")

classes_available[["invisible_blade"]] = list(class_name="Invisible Blade",
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
classes_available[["invisible_blade"]][["feats"]][[1 ]] = c("Bleeding Wound I")
classes_available[["invisible_blade"]][["feats"]][[3 ]] = c("Bleeding Wound II")
classes_available[["invisible_blade"]][["feats"]][[5 ]] = c("Bleeding Wound III",
                                                            "Feint Mastery")