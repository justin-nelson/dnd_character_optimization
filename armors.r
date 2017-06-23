shields_av = list("None"=list(),
                  "Regular"=list(),
                  "Tower"=list())
shields_av[["Regular"]][["Light Shield"]] = list(type="Regular",
                                                 armor_class=1)
shields_av[["Regular"]][["Heavy Shield"]] = list(type="Regular",
                                                 armor_class=2)
shields_av[["Tower"]][["Tower Shield"]] = list(type="Regular",
                                               armor_class=4)

armors_av = list("None"=list(),
                 "Light"=list(),
                 "Medium"=list(),
                 "Heavy"=list())
armors_av[["None"]][["Clothing"]] = list(type="None",
                               armor_class=0,
                               max_dex = 99,
                               spell_fail=0.00)
armors_av[["Light"]][["Padded Armor"]] = list(type="Light",
                                   armor_class=1,
                                   max_dex = 8,
                                   spell_fail=0.05)

armors_av[["Light"]][["Leather Armor"]] = list(type="Light",
                                    armor_class=2,
                                    max_dex = 6,
                                    spell_fail=0.10)


armors_av[["Light"]][["Studded Leather Armor"]] = list(type="Light",
                                            armor_class=3,
                                            max_dex = 5,
                                            spell_fail=0.15)

armors_av[["Light"]][["Chain Shirt"]] = list(type="Light",
                                  armor_class=4,
                                  max_dex = 4,
                                  spell_fail=0.20)

armors_av[["Medium"]][["Hide Armor"]] = list(type="Medium",
                                  armor_class=3,
                                  max_dex = 5,
                                  spell_fail=0.20)

armors_av[["Medium"]][["Scale Mail"]] = list(type="Medium",
                                 armor_class=5,
                                 max_dex = 3,
                                 spell_fail=0.25)
armors_av[["Medium"]][["Breastplate"]] = list(type="Medium",
                                 armor_class=5,
                                 max_dex = 3,
                                 spell_fail=0.25)
armors_av[["Medium"]][["Chainmail"]] = list(type="Medium",
                                armor_class=6,
                                max_dex = 2,
                                spell_fail=0.30)

armors_av[["Heavy"]][["Splint Mail"]] = list(type="Heavy",
                                  armor_class=7,
                                  max_dex = 0,
                                  spell_fail=0.40)
armors_av[["Heavy"]][["Banded Mail"]] = list(type="Heavy",
                                  armor_class=7,
                                  max_dex = 1,
                                  spell_fail=0.35)
armors_av[["Heavy"]][["Half-plate"]] = list(type="Heavy",
                                  armor_class=8,
                                  max_dex = 0,
                                  spell_fail=0.40)
armors_av[["Heavy"]][["Full Plate"]] = list(type="Heavy",
                                  armor_class=8,
                                  max_dex = 1,
                                  spell_fail=0.35)