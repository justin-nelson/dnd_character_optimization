source("feats.r")
source("class.r")
source("spells.r")
source("evaluateBuild.r")

g_casterLevel = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  casterLevel = g_casterProgression(build, options, class_n, feat_list)
  
  return(casterLevel)
}

g_casterProgression = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  casterProgression = g_classLevel(build, class_n)
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]][["caster_progression"]])
       & !is.null(feats_av[[feat_n]][["class_that_progresses"]])
       & !is.null(feats_av[[feat_n]][["class_to_progresses"]])){
      if(feats_av[[feat_n]][["class_to_progresses"]] == class_n){
        casterProgression = casterProgression + sum(feats_av[[feat_n]][["caster_progression"]] <= g_classLevel(build, feats_av[[feat_n]][["class_that_progresses"]]))
      }
    }
  }
  return(casterProgression)
}

g_bonusSpellSlotsByLevel = function(stat_mod, level){
  return(max(floor((stat_mod - level)/4)+1, 0))
}

g_bonusSpellSlots = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(cls_av[[class_n]][["spell_stat"]])) return(c())
  spell_stat = cls_av[[class_n]][["spell_stat"]]
  
  stat_mod = g_stat_mod(build, spell_stat, feat_list)
  
  bonus_spells = rep(0, 10)
  for(i in 2:10){
    bonus_spells[i] = g_bonusSpellSlotsByLevel(stat_mod, i-1)
  }
  return(bonus_spells)
}

g_spellslots = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  class_level = g_classLevel(build, class_n)
  spell_slots = cls_av[[class_n]][["spells"]][[class_level]] 
  spell_slots = spell_slots + g_bonusSpellSlots(build, options, class_n, feat_list)[1:length(spell_slots)]
  return(spell_slots)
}

g_spells = function(build, options, class_n, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  if(is.null(build$spell_build)) return(c())
  if(is.null(cls_av[[class_n]][["spell_build"]])) return(c())
  if(is.null(cls_av[[class_n]][["spell_build"]][[build$spell_build]])) return(c())
  
  spell_slots = g_spellslots(build, options, class_n)
  spell_list = c()
  if(length(spell_slots) == 0) return(c())
  for(i in (1:length(spell_slots))){
    if( length(cls_av[[class_n]][["spell_build"]][[build$spell_build]]) < i ) break
    if( is.null(cls_av[[class_n]][["spell_build"]][[build$spell_build]][[i]]) ) next
    spell_list_level_i = cls_av[[class_n]][["spell_build"]][[build$spell_build]][[i]]
    spell_list = c(spell_list, spell_list_level_i[1:min(length(spell_list_level_i), spell_slots[i])])
  }
  
  return(spell_list)
}

calculateBAB = function(build){
  BAB = 0
  if(calculateLevel(build) == 0) return(0)
  for(class_n in g_uniqClassList(build)){
    classLevel = g_classLevel(build, class_n)
    if(class_n == "any") next
    if(cls_av[[class_n]][["BAB"]] == "high"){ BAB = BAB + classLevel }
    if(cls_av[[class_n]][["BAB"]] == "medium"){ BAB = BAB + floor(0.75*classLevel) }
    if(cls_av[[class_n]][["BAB"]] == "low"){ BAB = BAB + floor(0.5*class_level) }
  }
  
  return(BAB)
}

calculateLevel = function(build){ 
  if(is.null(build$class)) return(0)
  return(length(g_classList(build)))
}

getAvailability = function(avail_classes, class){
  if(is.null(avail_classes[[class]])) { 
    return(0) 
  } else {
    if(is.na(avail_classes[[class]])){
      return(0)
    } else {
      return(avail_classes[[class]])
    }
  }
  
}

g_classLevel = function(build, class_n) {
  classLevels = rep(0, length(class_n))
  for(ind in 1:length(class_n)){
    classLevels[ind] = sum(g_classList(build) == class_n[ind], na.rm=T)
  }
  classLevels[is.na(classLevels)] = 0
  return(classLevels)
}

g_classFeats = function(build, class_n, level){
  if(is.null(cls_av[[class_n]][["feats"]])) return(c())
  feat_list = cls_av[[class_n]][["feats"]][1:level]
  return(unlist(feat_list, F, F))
}

###
### SetupFeatsRetrieve
###
# Ok this is silly but one of the most run functions is getting feats
# and because the feats are in vector form retrieving them is slow
# this sets up a way to quickly retrieve the feats.
setupFeatsRetrieve = function(class_av, max_level=30){
  class_list = names(class_av)
  for(class_n in class_list){
    if(is.null(class_av[[class_n]][["feats"]])) next
    class_av[[class_n]][["feats_at_lv"]] = list()
    class_av[[class_n]][["feats_at_lv"]][[1]] = class_av[[class_n]][["feats"]][[1]]
    for(lv in 2:max_level){
      if(length(class_av[[class_n]][["feats"]]) < lv) {
        class_av[[class_n]][["feats_at_lv"]][[lv]] = class_av[[class_n]][["feats_at_lv"]][[lv-1]]
      } else {
        class_av[[class_n]][["feats_at_lv"]][[lv]] = unique(c(class_av[[class_n]][["feats_at_lv"]][[lv-1]], class_av[[class_n]][["feats"]][[lv]]))
      }
    }
  }
  return(class_av)
}
cls_av = setupFeatsRetrieve(cls_av, 30)
GLOBAL_fastFeatRetrieve=T

g_grantingFeatList = function(feats_avail){
  feat_list = names(feats_avail)
  granting_feats = c()
  for(feat_n in feat_list){
    if(!is.null(feats_av[[feat_n]])){
      if(!is.null(feats_av[[feat_n]][["grant"]])){
        granting_feats = c(granting_feats, feat_n)
      }
    }
  }
  return(granting_feats)
}

GLOBAL_grantingFeats = g_grantingFeatList(feats_av)

g_classList = function(build){ return(build$class[!is.na(build$class)]) }
g_uniqClassList = function(build){ return(unique(g_classList(build))) }

g_feats = function(build){
  c_feats = c()
  ### Get the feats selected at level up
  c_feats = c(c_feats, unlist(build$feats, FALSE, FALSE))
  
  ### Get class feats
  class_list = g_classList(build)
  uniq_class_list = unique(class_list)
  #canDoFastRetrieve = F
  for(class_n in uniq_class_list){
    if(!GLOBAL_fastFeatRetrieve){
      if(is.null(cls_av[[class_n]][["feats"]])) next
    }
    class_level = sum(class_list == class_n, na.rm=T)
    if(GLOBAL_fastFeatRetrieve){
      class_level_to_retrieve = min(class_level, length(cls_av[[class_n]][["feats_at_lv"]]))
      feat_list = cls_av[[class_n]][["feats_at_lv"]][[class_level_to_retrieve]]
    } else {
      feat_list = unlist(cls_av[[class_n]][["feats"]][1:class_level])
    }
    c_feats = c(c_feats, feat_list)
  }
  
  ### Get autogrant feats, these are feats that are prereqs for classes and such
  granting_feats = c_feats[c_feats %in% GLOBAL_grantingFeats]
  for(feat_n in granting_feats){
    c_feats = c(c_feats, feats_av[[feat_n]][["grant"]])
  }
  
  if(length(c_feats) > 0) c_feats = c_feats[!is.na(c_feats)]
  
  return(c_feats)
}

hasAllFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(all(feat_n %in% feat_list)) 
}

hasAnyFeats = function(build, feat_n, feat_list=NULL){ 
  if(is.null(feat_list)) feat_list = g_feats(build)
  return(any(feat_n %in% feat_list)) 
}

g_classSkills = function(build, class_n){
  if(is.null(cls_av[[class_n]][["class_skills"]])) return(c())
  return(cls_av[[class_n]][["class_skills"]])
}
g_skillMaxes = function(build, skill_n){
  maxSkill = 3+calculateLevel(build)
  minSkill = floor(maxSkill/2)
  skill = rep(minSkill, length(skill_n))
  for(class_n in unique(build$class)){
    skill[skill_n %in% g_classSkills(build, class_n)] = maxSkill
  }
  return(skill)
}

preferredClassesUnmet = function(build, preferred_classes){
  unmet_pref = c()
  for(class_n in names(preferred_classes)){
    if(g_classLevel(build, class_n) < preferred_classes[[class_n]]) unmet_pref = c(unmet_pref, class_n)
  }
  return(unmet_pref)
}

CLASS_EXPLORATION_DEPTH = 3
calculateQualifiedClasses = function(avail_classes, build, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  qual_class = c()
  c_feats = g_feats(build)
  
  available_classes = names(cls_av)[names(cls_av) %in% names(avail_classes > 0)]
  for(class_n in available_classes){
    if(g_classLevel(build, class_n) >= avail_classes[[class_n]]) next # Are we past the predefined level?
    if(!is.null(cls_av[[class_n]][["prereq_BAB"]])){ if(cls_av[[class_n]][["prereq_BAB"]] > calculateBAB(build)) next } #BAB Check
    if(!is.null(cls_av[[class_n]][["prereq_feats"]])){ if(!hasAllFeats(build, cls_av[[class_n]][["prereq_feats"]])) next } # prereq Feat Check
    if(!is.null(cls_av[[class_n]][["prereq_skills"]])){ 
      if(!all(g_skillMaxes(build, names(cls_av[[class_n]][["prereq_skills"]])) > cls_av[[class_n]][["prereq_skills"]])) next
    }
    qual_class = c(class_n, qual_class)
  }
  
  class_table = sapply(3-table(build$class), max, 0)
  class_table = class_table[names(class_table) != "any"]
  
  ## If I already have 4 classes, I cannot take any more
  max_classes = 4
  if(prestigeSearch != "" & !(prestigeSearch %in% qual_class)) max_classes = 3 
  if(length(class_table) == max_classes)               { qual_class = qual_class[qual_class %in% build$class] }
  ## If I am above level 20, the 3b20 rule will not allow me to take more
  if(calculateLevel(build) >= 20){ qual_class = qual_class[qual_class %in% build$class] }
  
  ## 3b20 rule, if we are already about to run into it, I cannot select a new class.
  if(calculateLevel(build) >= 15 & calculateLevel(build) < 20){ 
    if(calculateLevel(build) + sum(class_table) >= 20){ 
      qual_class = qual_class[qual_class %in% names(which(class_table > 0))]
    }
    if(calculateLevel(build) + sum(class_table) + 3 >= 21){ 
      qual_class = qual_class[qual_class %in% build$class]
    }
  }
  ########################################################################################################
  ### Restrict to prestigeClass
  ########################################################################################################
  if(any(qual_class == prestigeSearch, na.rm=T)) qual_class = c(prestigeSearch)
  
  ########################################################################################################
  ### Restrict to preferred classes
  ########################################################################################################
  # This is useful for things such as class search.
  unmet_pref = preferredClassesUnmet(build, preferred_classes)
  if(any(qual_class %in% unmet_pref, na.rm=T)) qual_class = qual_class[qual_class %in% unmet_pref]
  
  ########################################################################################################
  ### Restrict the Branching Factor
  ########################################################################################################
  # classes are problematic in that they greatly add to the branching factor. In order to restrict this
  # I perform a branch cutting algorithm here.
  #qual_class = reduceClassBranchFactor(avail_classes, build, qual_class, min(calculateLevel(build)+CLASS_EXPLORATION_DEPTH, 30), cooling, options)
  #qual_class = reduceClassBranchFactor(avail_classes, build, qual_class, max_level, cooling, options, T)
  
  if(length(qual_class) == 0) return(c("any"))
  return(qual_class)
}

reduceClassBranchFactor = function(avail_classes, build, qualified_classes, max_level=calculateLevel(build)+1, cooling=0.9, options=list(), is.final=T, base_build=0){
  if(length(qualified_classes) == 0) { build$class[calculateLevel(build)+1:max_level] = "any" }
  if(calculateLevel(build) >= max_level) { return(qualified_classes) }  
  
  scores = list()
  for(class_n in qualified_classes){
    branch_build = createBranchBuild(avail_classes, build, qualified_classes, max_level, cooling, class_n)
    branch_build = populateFeats(avail_classes, branch_build, cooling, options)
    global_evaluations_3 <<- global_evaluations_3 + 1
    scores[class_n] = evaluateBuild(branch_build, options, calculateLevel(branch_build), max_level - calculateLevel(build) + 1, base_build)$score
  }
  reduced_list = names(which(scores >= cooling * max(unlist(scores, F, F))))
  return(reduced_list)
}


getChoosableFeatList = function(feats_av){
  feat_list = c()
  for(feat_n in names(feats_av)){
    if(is.null(feats_av[[feat_n]][["feat_type"]])) next
    if(length(feats_av[[feat_n]][["feat_type"]]) == 0) next
    feat_list = c(feat_list, feat_n)
  }
  return(feat_list)
}

# g_scoreIncreasingFeats = function(build, feat_list, options=list()){
#   good_feats = c()
#   base_score = evaluateBuild(build, options, calculateLevel(build), 1)$score
#   for(ind in 1:length(feat_list)){
#     feat_n = feat_list[ind]
#     scoring_build = build
#     scoring_build$feats[[length(scoring_build$feats)+1]] = c(feat_n)
#     global_evaluations_4 <<- global_evaluations_4 + 1
#     score = evaluateBuild(scoring_build, options, calculateLevel(scoring_build), 1)$score
#     if(score > base_score){
#       good_feats = c(good_feats, feat_n)
#     }
#   }
#   return(good_feats)
# }

g_scoreFeats = function(build, feat_list, options=list()){
  scores = rep(0, length(feat_list))
  for(ind in 1:length(feat_list)){
    feat_n = feat_list[ind]
    scoring_build = build
    scoring_build$feats[[length(scoring_build$feats)+1]] = c(feat_n)
    global_evaluations_4 <<- global_evaluations_4 + 1
    scores[ind] = evaluateBuild(scoring_build, options, calculateLevel(scoring_build), 1)$score
  }
  return(scores)
}

g_orderedBestFeats = function(build, options=list()){
  feat_list = getChoosableFeatList(feats_av)
  scores = g_scoreFeats(build, feat_list, options)
  feat_list = feat_list[order(scores, decreasing=T)]
  scores = scores[order(scores, decreasing=T)]
  return(feat_list)
}

#############################################
### PopulateFeats: return build
###############################################
# Sometimes I have a build that does not have feats
# This function populates the feat list, with greedy optimization
populateFeats_callCount = 0
populateFeats = function(avail_classes, build, cooling=0.9, options=list()){
  if(length(build$feats) > calculateLevel(build)) return(build)
  populateFeats_callCount <<- populateFeats_callCount + 1
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(build, options)
  
  for(i in length(build$feats):calculateLevel(build)){
    level_build = createPrefixBuild(build, calculateLevel(build) - i, F)
    new_feats = calculateNumberOfNewFeats(level_build)
    last_class = lastCharacterClass(level_build)
    if(sum(unlist(new_feats)) > 0){ # There should be a feat at this level
      if(length(build$feats) < i){ # But if there isn't, we need to add some ... optimally
        feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
        level_build$feats[[i]] = rep(NA, 1)
        for(j in 1:length(feat_types)){
          level_build$feats[[i]][j] = options$ord_feat_list[which(options$ord_feat_list %in% calculateQualifiedFeats(level_build, feat_types[j], ""))[1]]
        }
        build$feats[[i]] = level_build$feats[[i]]
      }
    }
  }
  return(build)
}

resetBuild = function(){
  build = createPrefixBuild(windmaster_char, 15, F)
  return(build)
}

createBranchBuild = function(avail_classes, branch_build, qualified_classes, max_level=calculateLevel(build)+1, cooling=0.9, class_n="any"){
  available_levels = getAvailability(avail_classes, class_n) - g_classLevel(branch_build, class_n) 
  max_ind = min(calculateLevel(branch_build) + available_levels, max_level)
  branch_build$class[(calculateLevel(branch_build)+1):max_ind] = class_n
  
  if(calculateLevel(branch_build) < max_level){
    branch_build = createBranchBuild(avail_classes, 
                                     branch_build, 
                                     qualified_classes[qualified_classes != class_n], 
                                     max_level,
                                     cooling,
                                     class_n)
  }
  return(branch_build)
}

lastCharacterClass = function(build){ 
  last_class_index = which(is.na(build$class))[1]-1
  if(is.na(last_class_index)) return(build$class[length(build$class)])
  return(build$class[which(is.na(build$class))[1]-1]) 
}

createStatIncreaseList = function(feats_av){
  feat_stat_increases_list = list("Strength"=c(),
                                  "Dexterity"=c(),
                                  "Constitution"=c(),
                                  "Intelligence"=c(),
                                  "Wisdom"=c(),
                                  "Charisma"=c())
  
  for(feat_n in names(feats_av)){
    for(stat_n in names(feat_stat_increases_list)){
      if(!is.null(feats_av[[feat_n]][[paste("plus", stat_n, sep="")]])) { feat_stat_increases_list[[stat_n]] = c(feat_stat_increases_list[[stat_n]], feat_n) }
    }
  }
  
  return(feat_stat_increases_list)
}

feats_to_increase_stats = createStatIncreaseList(feats_av)

g_stats = function(build, stats, feat_list=NULL){
  if(is.null(feat_list)) feat_list = g_feats(build)
  base_stats = build$stats[stats]
  for(stat_n in names(base_stats)){
    base_stats[stat_n] = base_stats[stat_n] + sum(feat_list %in% feats_to_increase_stats[[stat_n]])
  }
  
  if(!is.null(build$stat_increase)){ base_stats[build$stat_increase] = base_stats[build$stat_increase] + floor(calculateLevel(build) / 4) }
  return(unlist(base_stats[stats]))
}

g_featsInFeatType = function(feat_type){
  feat_list = c()
  for(feat_n in names(feats_av)){
    if( is.null(feats_av[[feat_n]][["feat_type"]])        ) next
    if( !feat_type %in% feats_av[[feat_n]][["feat_type"]] ) next
    feat_list = c(feat_list, feat_n)
  }
  return(feat_list)
}

g_featPrereqs = function(feat_n){
  prereqs = c()
  if(is.null(feats_av[[feat_n]])) return(prereqs)
  if(is.null(feats_av[[feat_n]][["prereq_feats"]])) return(prereqs)
  
  prereqs = feats_av[[feat_n]][["prereq_feats"]]
  if(length(prereqs) > 0){
    for(feat_n2 in prereqs){
      prereqs = c(prereqs, g_featPrereqs(feat_n2))
    }
  }
  
  return(prereqs)
}

g_prestigeFeatRequirements = function(build, prestigeSearch=""){
  if(prestigeSearch == "") return(c())
  if(is.null(cls_av[[prestigeSearch]])) return(c())
  if(is.null(cls_av[[prestigeSearch]][["prereq_feats"]])) return(c())
  
  prereq_feats = cls_av[[prestigeSearch]][["prereq_feats"]]
  prereq_feats_fin = prereq_feats
  for(feat_n in prereq_feats){
    prereq_feats_fin = c(prereq_feats_fin, g_featPrereqs(feat_n))
  }
  
  prereq_feats = unique(prereq_feats_fin)
  prereq_feats = prereq_feats[!prereq_feats %in% g_feats(build)]
  
  return(prereq_feats)
}

calculateQualifiedFeats = function(build, feat_type, prestigeSearch=""){
  qual_feats = c()
  
  available_feats = g_featsInFeatType(feat_type)
  available_feats = available_feats[!available_feats %in% g_feats(build)]
  for(feat_n in g_featsInFeatType(feat_type)){
    if( !is.null(feats_av[[feat_n]][["prereq_level"]])  ){ if(feats_av[[feat_n]][["prereq_level"]] > calculateLevel(build)) next } # Level Check
    if( !is.null(feats_av[[feat_n]][["prereq_BAB"]])    ){ if(feats_av[[feat_n]][["prereq_BAB"]] > calculateBAB(build)) next } # BAB Check
    if( !is.null(feats_av[[feat_n]][["prereq_stat"]])   ){
      if(any(g_stats(build, names(feats_av[[feat_n]][["prereq_stat"]])) < feats_av[[feat_n]][["prereq_stat"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_feats"]])  ){ if(!hasAllFeats(build, feats_av[[feat_n]][["prereq_feats"]])) next } # prereq Feat Check
    if( !is.null(feats_av[[feat_n]][["prereq_skills"]]) ){ 
      if(any(g_skillMaxes(build, names(feats_av[[feat_n]][["prereq_skills"]])) < feats_av[[feat_n]][["prereq_skills"]])) next
    }
    if( !is.null(feats_av[[feat_n]][["prereq_class"]])  ){
      prereq_classes_met = T
      for(class_n in names(feats_av[[feat_n]][["prereq_class"]])){
        if(g_classLevel(build, class_n) < feats_av[[feat_n]][["prereq_class"]][[class_n]]) prereq_classes_met = F
      }
      if(!prereq_classes_met) next
    }
    qual_feats = c(qual_feats, feat_n)
  }
  
  qual_feats = qual_feats[!qual_feats %in% g_feats(build)]
  
  ########################################################################################################
  ### Restrict to feats which qualify me for prestigeSearch
  ########################################################################################################
  # This is useful for things such as prestige search.
  unmet_pref = g_prestigeFeatRequirements(build, prestigeSearch)
  if(any(qual_feats %in% unmet_pref, na.rm=T)) qual_feats = qual_feats[qual_feats %in% unmet_pref]
  
  if(length(qual_feats) == 0) return(c("any"))
  return(qual_feats)
}

calculateNumberOfNewFeats = function(build){
  new_feats = list("Regular"=0)
  
  if(calculateLevel(build) == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) == 1 & build$isHuman) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) <= 20 & calculateLevel(build) %% 3 == 0) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  if(calculateLevel(build) >  20 & calculateLevel(build) %% 2 == 1) new_feats[["Regular"]] = new_feats[["Regular"]]+1
  
  last_class = lastCharacterClass(build)
  last_level = g_classLevel(build, last_class)
  
  new_feats[[last_class]] = 0
  if(!is.null(cls_av[[last_class]][["bonus_feats"]])){
    new_feats[[last_class]] = sum(cls_av[[last_class]][["bonus_feats"]] == last_level)
  }
  
  return(new_feats)
}

MONK_WEAPON_LIST = c("Kama", "Unarmed")

g_stat_enhancement_mod = function(build, stat, feat_list=NULL, useSpells=FALSE){
  if(is.null(feat_list)) feat_list = g_feats(build)
  
  equipment_bonus = build$equipment_stats
  enhancement_bonus = equipment_bonus
  if(useSpells){
    for(feat_n in feat_list){
      if(is.null(feats_av[[feat_n]])) next
      if(is.null(feats_av[[feat_n]]$enhancement_bonus)) next
      spell_bonus = feats_av[[feat_n]]$enhancement_bonus(build, options, feat_list, equipment_bonus)
      enhancement_bonus = pmax(enhancement_bonus, spell_bonus)
    }
  }
  return(enhancement_bonus)
}
g_stat_mod = function(build, stat, feat_list=NULL, useSpells=FALSE){
  if(is.null(feat_list)) feat_list = g_feats(build)
  stats = g_stats(build, stat, feat_list) + unlist(build$equipment_stats[stat])
  return(floor((stats - 10) / 2))
}

outputCharacter = function(build, options=list()){
  cat("\n\n")
  char_table = table(build$class)
  cat(paste(names(char_table), char_table,  sep=" ", collapse=" / "))
  cat("\n")
  cat(paste(1:30, ": ", build$class, sep="", collapse=", "))
  cat("\n")
  cat(paste(1:length(build$feats), ": ", build$feats, sep="", collapse="\n"))
  cat("\n")
  build.evaluation = evaluateBuild(build, options, calculateLevel(build))
  cat("best_score: ")
  cat(build.evaluation$score)
  cat("\n\n")
  cat("damage by level|")
  cat(paste(1:30, ": ", format(build.evaluation$damage, digits=3), sep="", collapse=", "))
  cat("\n\n")
}

levelClass = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  cur_lv = calculateLevel(build)
  global_class_selections <<- global_class_selections + 1
  if(length(build$potential_classes) < cur_lv+1){
    build$potential_classes[[cur_lv+1]] = calculateQualifiedClasses(avail_classes, build, preferred_classes, prestigeSearch, max_level, cooling, options)
  }
  class_index = sample(1:length( build$potential_classes[[cur_lv+1]] ), 1)
  class_name = build$potential_classes[[cur_lv+1]][class_index]
  build$class[cur_lv+1] = class_name
  build$potential_classes[[cur_lv+1]] = build$potential_classes[[cur_lv+1]][-1 * class_index]
  return(build)
}

selectFeat = function(build, number, feat_type="Regular", prestigeSearch=""){
  cur_lv = calculateLevel(build)
  if(length(build$potential_feats) < cur_lv){
    build$potential_feats[[cur_lv]] = list()
  } else if(is.null(build$potential_feats[[cur_lv]])){
    build$potential_feats[[cur_lv]] = list()
  }
  if(length(build$potential_feats[[cur_lv]]) < number){
    build$potential_feats[[cur_lv]][[number]] = calculateQualifiedFeats(build, feat_type, prestigeSearch) 
  }
  if(number == 1) {
    build$feats[[cur_lv]] = rep(NA, 1)
    build$feat_types[[cur_lv]] = rep(NA, 1)
  }
  
  feat_ind = sample(1:length( build$potential_feats[[cur_lv]][[number]] ), 1)
  feat_name = build$potential_feats[[cur_lv]][[number]][feat_ind]
  build$feats[[cur_lv]][number] = feat_name
  build$potential_feats[[cur_lv]][[number]] = build$potential_feats[[cur_lv]][[number]][-1 * feat_ind]
  build$feat_types[[cur_lv]][[number]] = feat_type
  
  return(build)
}

levelFeat = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9){
  new_feats = calculateNumberOfNewFeats(build)
  last_class = lastCharacterClass(build)
  if(sum(unlist(new_feats)) > 0){
    global_feat_selections <<- global_feat_selections + 1
    
    feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
    for(i in 1:sum(unlist(new_feats))){
      build = selectFeat(build, i, feat_types[i], prestigeSearch)
    }
  } else {
    build$potential_feats[[calculateLevel(build)]] = list()
  }
  return(build)
}

levelup = function(build, avail_classes, preferred_classes=list(), prestigeSearch="", max_level=calculateLevel(build)+1, cooling=0.9, options=list()){
  build = levelClass(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
  build = levelFeat(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling)
  return(build)
}

delevel = function(build, avail_classes, cooling, options=list(), prestigeSearch=""){
  while(calculateLevel(build) > 0){
    if(length(build$potential_feats) < calculateLevel(build)){ # There was no feat at this level, defeat
      build$class[calculateLevel(build)]             = NA
    } else {
      new_feats = calculateNumberOfNewFeats(build)
      last_class = lastCharacterClass(build)
      if(sum(unlist(new_feats)) > 0){
        for(i in sum(unlist(new_feats)):1){ # set empty potential feat lists to null
          if(length(build$potential_feats[[calculateLevel(build)]]) != 0){
            if(length(build$potential_feats[[calculateLevel(build)]][[i]]) == 0){ 
              build$potential_feats[[calculateLevel(build)]][i] = NULL
            } else {
              break
            }
          }
        }
      }
      
      if(length(build$potential_feats[[calculateLevel(build)]]) == 0){ # if there are no more feats for this level
        build$feats[[calculateLevel(build)]]           = NULL
        build$potential_feats[[calculateLevel(build)]] = NULL
        build$class[calculateLevel(build)]             = NA
      } else {
        #  build$feat_types[[calculateLevel(build)]][i])
        feat_types = c(rep(last_class, new_feats[[last_class]]), rep("Regular", new_feats[["Regular"]]))
        for(i in length(build$potential_feats[[calculateLevel(build)]]):sum(unlist(new_feats))){ build = selectFeat(build, i, feat_types[i], prestigeSearch) } # Relevel
        return(build)
      }
    }
    
    # If the code made it this far, I need to figure out if there are more classes to take, if so level up and break
    if(length(build$potential_classes) >= calculateLevel(build)+1){ # Well, there's something there
      if(length(build$potential_classes[[calculateLevel(build)+1]]) >= 1){ # Is there a class to take
        build = levelup(build, avail_classes, prestigeSearch=prestigeSearch, cooling=cooling, options=options)
        return(build)
      } else { # No class to take, set potential classes to NULL and delevel again
        if(length(build$potential_classes) >= calculateLevel(build)+1){
          build$potential_classes[[calculateLevel(build)+1]] = NULL
        }
      }
    }
  }
  return(build)
}

g_baseClass = function(){
  base_class_list = c()
  for(class_n in names(cls_av)){
    if(is.null(cls_av[[class_n]][["base"]])) next
    if(!cls_av[[class_n]][["base"]]) next
    base_class_list = c(base_class_list, class_n)
  }
  return(base_class_list)
}

g_aveDamageDice = function(dice, sides){ return((dice + dice*sides)/2) }

build_character = function(avail_classes, 
                           max_level = 30, 
                           level_warp = 1, 
                           options=list(), 
                           is.final=T, 
                           prefix_build = blank_prefix_build, 
                           monoclass_depth=5, 
                           cooling=1, 
                           report=T, 
                           preferred_classes=list(), 
                           prestigeSearch=""){
  
  
  builds_analyzed = 0
  
  best_build = blank_prefix_build
  best_score = -9999
  best_build_scores = rep(0, 30)
  
  if(is.null(options$ord_feat_list)) options$ord_feat_list = g_orderedBestFeats(best_build, options)
  
  if(max_level > calculateLevel(prefix_build) + level_warp){
    build = build_character(avail_classes, 
                            max_level=max_level-1, 
                            level_warp = level_warp, 
                            options = options, 
                            is.final=F, 
                            prefix_build=prefix_build, 
                            monoclass_depth = monoclass_depth, 
                            report=report, 
                            cooling=cooling,
                            preferred_classes=preferred_classes,
                            prestigeSearch=prestigeSearch)
  } else {
    build = prefix_build
  }
  if(max_level - level_warp <= 0){ return(build) }
  
  base_score = evaluateBuild(build, options, calculateLevel(build), level_warp)$score
  if(report) cat(max_level, "base_score:", base_score, "\n")
  
  start = T
  max_level_reached = F
  while(length(build$potential_classes) != 0 || start){
    start = F
    
    ################################################################
    # Level up the character to 30 so I can properly assess damage
    ##################################################################
    while(calculateLevel(build) < max_level){
      if( max_level_reached & calculateLevel(build) > 0) {
        global_evaluations_1 <<- global_evaluations_1 + 1
        eval_score = best_build_scores[calculateLevel(build)]
        if(eval_score < 0) { eval_score = eval_score/cooling } else { eval_score = eval_score * cooling }
        if(evaluateBuild(build, options, calculateLevel(build), level_warp, base_score)$score < eval_score ) { break; }
      }
      build = levelup(build, avail_classes, preferred_classes, prestigeSearch, max_level, cooling, options)
    }
    
    ##########################################
    # Debug Messages
    ##########################################
    # print(builds_analyzed)
    # cat(build$class[!is.na(build$class)], "\n")
    # if(builds_analyzed %% 10 == 0){ cat(build$class[!is.na(build$class)], "\n")}
    
    ##########################################
    # Assess
    ##########################################
    if(calculateLevel(build) == max_level){
      
      max_level_reached = T
      builds_analyzed = builds_analyzed + 1
      if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
      global_evaluations_2 <<- global_evaluations_2 + 1
      build.evaluation = evaluateBuild(build, options, calculateLevel(build), level_warp+1, base_score)
      
      if(build.evaluation$score > best_score){
        best_build = build
        best_score = build.evaluation$score
        if(report) cat(max_level, "score:", best_score, "\n")
        #if(report) print(build$feats)
        for(i in 1:calculateLevel(build)){
          global_evaluations_5 <<- global_evaluations_5 + 1
          best_build_scores[i] = evaluateBuild(best_build, options, i, level_warp+1, base_score)$score
        }
        best_build_scores
      }
    }
    
    ##########################################
    # Delevel
    ##########################################
    build = delevel(build, avail_classes, cooling, options, prestigeSearch)
    
  }
  
  #################################################
  # Monoclass Search
  #
  ### This is the second chain of the search. Instead of using a greedy approach to restrict the search space
  ### I am restricting the search space based on only looking at a single class. This should allow for a much
  ### deeper search. This is a good search to do because many class features require many class levels
  ### in order to function.
  #
  ### It is possible that the best build comes from this "deep" search instead of the shallow level warp search.
  ### Note, I will still level warp afterwards, however the next shallow search should easily find the new
  ### local maxima.
  #
  ### Many prestige classes have requirements which are not good however the class
  ### positively contributes to the evaluation function. Local optimization will never allow
  ### the prereqs to be taken so I need to do a "dive" for the prestige class.
  #################################################
  global_build <<- best_build
  global_max_level <<- max_level
  base_class_build = createPrefixBuild(best_build, monoclass_depth, F)
  if(level_warp+1 <= monoclass_depth 
     & length(g_uniqClassList(base_class_build)) < 4 
     & calculateLevel(base_class_build) <= 20 
     & max_level > level_warp+1){
    
    if(report) cat("monoclass score:", best_score, "\n")
    for(class_n in names(which(avail_classes > 0))){
      # Also make sure I have levels of the class left. If I don't have them here then there is no use going back.
      if(g_classLevel(best_build, class_n) >= getAvailability(avail_classes, class_n)) next
      if(class_n == "any") next
      depth_to_search = max(min(monoclass_depth, calculateLevel(best_build), max_level), level_warp+1)
      ##########################################
      # Start descending
      ##########################################
      for(i in (level_warp+1):depth_to_search){
        class_build = createPrefixBuild(best_build, i, F)
        base_score = evaluateBuild(class_build, options, calculateLevel(build), monoclass_depth+1)$score
        best_score = evaluateBuild(best_build, options, max_level, monoclass_depth+1, base_score)$score
        if(report) cat(calculateLevel(best_build), "|" , class_n, i, "| best_score:", best_score)
        class_build = build_character(avail_classes,
                                      max_level,
                                      0,
                                      options,
                                      prefix_build = class_build,
                                      monoclass_depth = 0,
                                      report=F,
                                      cooling=cooling,
                                      preferred_classes=preferred_classes,
                                      prestigeSearch=class_n)
        builds_analyzed = builds_analyzed + 1
        new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1, base_score)$score
        if(report) cat(" | score:", new_score, "\n")
        if(report) { if(builds_analyzed %% 1000 == 0) cat("builds_analyzed:", builds_analyzed, "\n") }
        if(new_score > best_score){
          class_build = createPrefixBuild(best_build, i, F)
          class_build = build_character(avail_classes,
                                        max_level,
                                        level_warp,
                                        options,
                                        prefix_build = class_build,
                                        monoclass_depth = 0,
                                        report=F,
                                        cooling=cooling,
                                        preferred_classes=preferred_classes,
                                        prestigeSearch=class_n)
          new_score = evaluateBuild(class_build, options, max_level, monoclass_depth+1, base_score)$score
          if(new_score > best_score){ # Still the best build?
            print("Mono search found a new build!")
            best_build = class_build
            best_score = new_score
            if(report) cat("mono score:", best_score, "\n")
          }
        }
      }
    }
  }
  
  if(report) cat(calculateLevel(best_build), ":", best_build$class[!is.na(best_build$class)], "\n")
  
  #cat("level:", calculateLevel(best_build), "level_warp:", level_warp, "prefix_level:", calculateLevel(best_build) - level_warp,"\n")
  best_build = createPrefixBuild(best_build, level_warp, is.final)
  
  return(best_build)
}

createPrefixBuild = function(build, levels_to_lose, is.final){
  build$potential_classes = lapply(build$potential_classes, function(x) x = character(0))
  build$potential_feats   = lapply(build$potential_feats, function(x) x = list())
  
  if(calculateLevel(build) > levels_to_lose & !is.final & levels_to_lose != 0){
    blankRange = (calculateLevel(build) - levels_to_lose + 1):30
    
    build$feats[blankRange] = NULL
    build$potential_feats[blankRange] = NULL
    build$potential_classes[blankRange] = NULL
    build$class[blankRange] = NA
  }
  
  return(build)
}

# Rprof("CO_profiling.out")


global_feat_selections = 0
global_class_selections = 0
global_evaluations = 0
global_evaluations_1 = 0
global_evaluations_2 = 0
global_evaluations_3 = 0
global_evaluations_4 = 0
global_evaluations_5 = 0

blank_prefix_build = list(class = rep(NA, 30),
                          feats = list(),
                          feat_types = list(),
                          potential_classes = list(),
                          potential_feats = list(),
                          weapon = list(name="Kama",
                                        damage=g_aveDamageDice(1, 6),
                                        size="Light",
                                        hands="Two-Weapon",
                                        damage_type="Slashing",
                                        enhancement_bonus=4,
                                        attack_bonus=0),
                          stats=c("Strength"=8,
                                     "Dexterity"=20,
                                     "Constitution"=10,
                                     "Intelligence"=16,
                                     "Wisdom"=10,
                                     "Charisma"=10),
                          equipment_stats=c("Strength"=2,
                                               "Dexterity"=2,
                                               "Constitution"=2,
                                               "Intelligence"=2,
                                               "Wisdom"=2,
                                               "Charisma"=2),
                          stat_increase="Dexterity",
                          favored_class="Fighter",
                          spell_build="Offensive Melee",
                          isHuman=F)

is.final = T
max_level = 30
level_warp = 5
monoclass_depth = 11
cooling = 0.9
options=list(allow_burst=T,
             allow_spells=T,
             allow_favoredEnemy=T,
             feat_adjustment=0.0001,
             feat_multiplier=0.01) # How much score is 1 feat worth
avail_classes = list("Monk"=11, "Dervish"=10, "Invisible Blade"=5, "Fighter"=0, "Swashbuckler"=0, "Tempest"=5, "Ranger"=30, "Whirling Dervish"=10)
windmaster_char = build_character(avail_classes,
                                  max_level = max_level,
                                  level_warp = level_warp,
                                  monoclass_depth = monoclass_depth,
                                  cooling = cooling,
                                  options = options)

# Rprof(NULL)
# summaryRprof("CO_profiling.out")

outputCharacter(windmaster_char, options)

