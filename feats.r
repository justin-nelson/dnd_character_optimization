feats_av = list()
feats_av[["Template"]] = list(name="Template",
                              feat_type=c("Fighter"),
                              prereq_level=31,
                              prereq_BAB=31,
                              prereq_stat=list("Strength"=100,
                                               "Dexterity"=100,
                                               "Constitution"=100,
                                               "Intelligence"=100,
                                               "Wisdom"=100,
                                               "Charisma"=100),
                              prereq_feat=c("Dodge"),
                              prereq_skills=list("any"=100),
                              prereq_class=list("fighter"=100),
                              autogrant=c("Template 2"))

feats_av[["Two-Weapon Fighting"]] = list(feat_type=c("Fighter"),
                                         prereq_stat=list("Dexterity"=15))
feats_av[["Improved Two-Weapon Fighting"]] = list(feat_type=c("Fighter"),
                                                  prereq_BAB=6,
                                                  prereq_stat=list("Dexterity"=17),
                                                  prereq_feat=c("Two-Weapon Fighting"))
feats_av[["Greater Two-Weapon Fighting"]] = list(feat_type=c("Fighter"),
                                                 prereq_level=0,
                                                 prereq_BAB=11,
                                                 prereq_stat=list("Dexterity"=19),
                                                 prereq_feat=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting"))
feats_av[["Perfect Two-Weapon Fighting"]] = list(feat_type=c("Fighter"),
                                                 prereq_level=21,
                                                 prereq_stat=list("Dexterity"=25),
                                                 prereq_feat=c("Two-Weapon Fighting", "Improved Two-Weapon Fighting", "Greater Two-Weapon Fighting"))
feats_av[["Dodge"]] = list(feat_type=c("Fighter"),
                           prereq_stat=list("Dexterity"=13))
feats_av[["Mobility"]] = list(feat_type=c("Fighter"),
                              prereq_stat=list("Dexterity"=13),
                              prereq_feat=c("Dodge"))
feats_av[["Spring Attack"]] = list(feat_type=c("Fighter"),
                                   prereq_stat=list("Dexterity"=13),
                                   prereq_feat=c("Dodge", "Mobility"),
                                   prereq_BAB=4)
feats_av[["Combat Expertise"]] = list(feat_type=c("Fighter"),
                                      prereq_stat=list("Intelligence"=13))
feats_av[["Whirlwind Attack"]] = list(feat_type=c("Fighter"),
                                      prereq_stat=list("Dexterity"=13,
                                                       "Intelligence"=13),
                                      prereq_feat=c("Dodge", "Mobility", "Spring Attack", "Combat Expertise"),
                                      prereq_BAB=4)
feats_av[["Weapon Focus (Kukri)"]] = list(feat_type=c("Fighter"),
                                          prereq_BAB=1,
                                          prereq_feat=c("Weapon Proficiency (Martial)"),
                                          grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)", "Weapon Focus (Invisible Blade)"))
feats_av[["Weapon Focus (Kama)"]] = list(feat_type=c("Fighter"),
                                         prereq_BAB=1,
                                         prereq_feat=c("Weapon Proficiency (Monk)"),
                                         grant=c("Weapon Focus (Slashing)", "Weapon Focus (Any)"))
feats_av[["Weapon Specialization (Kama)"]] = list(feat_type=c("Fighter"),
                                                  prereq_BAB=4,
                                                  prereq_feat=c("Weapon Focus (Kama)"),
                                                  prereq_class=list("fighter"=4))
feats_av[["Greater Weapon Focus (Kama)"]] = list(feat_type=c("Fighter"),
                                                 prereq_feat=c("Weapon Focus (Kama)"),
                                                 prereq_class=list("fighter"=8))
feats_av[["Greater Weapon Specialization (Kama)"]] = list(feat_type=c("Fighter"),
                                                          prereq_feat=c("Weapon Specialization (Kama)", "Greater Weapon Focus (Kama)"),
                                                          prereq_class=list("fighter"=12))
feats_av[["Epic Weapon Focus (Kama)"]] = list(feat_type=c("Fighter"),
                                              prereq_level=21,
                                              prereq_feat=c("Greater Weapon Focus (Kama)"))
feats_av[["Epic Weapon Specialization (Kama)"]] = list(feat_type=c("Fighter"),
                                                       prereq_level=21,
                                                       prereq_feat=c("Epic Weapon Focus (Kama)", "Greater Weapon Specialization (Kama)"))
feats_av[["Melee Weapon Mastery (Slashing)"]] = list(feat_type=c("Fighter"),
                                                     prereq_class=list("fighter"=12))
feats_av[["Weapon Finesse"]] = list(feat_type=c("Fighter"),
                                    prereq_BAB=1)
feats_av[["Epic Prowess"]] = list(feat_type=c("Fighter"),
                                  prereq_level=21)