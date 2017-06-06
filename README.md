# dnd_character_optimization

NOTE: this build is for BGTSCC

This is a computationally optimal kaze build allowing for the following classes:

Monk
Fighter
Dervish
Invisible Blade
Weapon Master
Swashbuckler

and the following feats:

Two Weapon Fighting, ITWF, GTWF, PTWF
Weapon Finesse
Weapon Focus (Kama), Weapon Focus (Kukri), Weapon Specialization (Kama), GWF(Kama), GWS(Kama), EWF(Kama), EWS(Kama), Weapon Mastery(slashing), Improved Critical(Kama)
Combat Expertise, Deadly Defense, Feint
Dodge, Mobility, SpringAttack, Whirlwind Attack
Powerful Charge, Epic Charge
Epic Prowess, Combat Insight
Great Dexterity 1

I had to try a couple of algorithms in order to get the build. In essence all of the algorithms are tree climbing algorithms similar to alpha-beta pruning commonly used in chess. The tree is approximately depth 60 + 1/2 fighter level with a branching factor hitting around 5. This means the search space is around 10^46 and guaranteed optimal solutions are difficult.

My original evaluation function was similar to distance based evaluations. I simply looked at the damage of the current build and if that was far enough away from the current optimal I pruned that branch of the tree. The threshold for pruning was dependant on level, higher levels mean that I prune more frequently than lower levels. However, this evaluation was extremely sensitive to local minima so I abandoned it in favor of a level warping approach.

The level warping approach is similar to a greedy approach, however I take some time to look ahead. I calculate the optimal level n build by optimizing the level n + level warp + 1 build while level 1:n-1 is fixed. This means that my search space at each level is 6 ^ (2 * level warp + <fighter levels/2, if any>) and my total computational time is (30 - level warp ) * 6 ^ (2 * level warp + <ave. fighter levels in level warp>). This is tractable for level warps upto about 4 or 5 (but will take a while to run).

The evaluation function that I am using to optimize the build is analagous to maximizing average damage over time. I simply take the sum_of damage_per_level * level as my evaluation function. I also have a parameter which controls how often I am able to achieve "burst" damage, which is when usable feats such as A Thousand Cuts, Dervish Dance and charge are used in the damage calculations. This is called burst_weight. My final evaluation function is:

score = burst weight * sum_of(burst_dam_per_level * level)
+ (1-burst_weight) * sum_of(sustain_dam_per_level * level)

Charge

This feat is given to all characters at level 1 for free. Activate at will. For 1 round, the character gains double movement, susceptibility to piercing attacks and a limit of 1 attack at +2 AB and -2 AC. Can only be activated while moving towards an enemy, that is neither too far nor too close.

Resist Poison = Powerful Charge

Each attack you when using charge gains a +2D6 damage bonus. Halflings and Gnomes gains a +D8 damage bonus instead. This damage applies to Shield Bash as well when charging.

Epic Skill Focus (Discipline) = Dire Charge

The character can use all their attacks, instead of 1 when using charge

Background = Furious Charger

The character gains a +4 AB bonus instead of the normal +2 when using charge

I have known the optimal build is Fighter 12/Monk 3/ Dervish 10/ Invisible Blade 5 from earlier calculations. However, finding it is difficult due to the high prerequisites of Invisible Blade. I am quite happy with how this turned out. Dervish at level 10 is interesting. Because Fighter doesn't give you anything at level 9 or 10 -- right after fighter 8 the build takes dervish. This is serendipitous because the build needs Dervish in order to grab Invisible blade. At level 11, the build sees invisible blade and takes fighter levels in order to qualify for the class. However, the build can only take 3 levels of invisible blade before it "sees" the 3b20 rule and needs to take classes to fulfill that. At level 20, the next goal is the fighter bonus feat to get combat insight as quickly as possible. I think I am going to rerun this with 20 starting dex at some point to see if PTWF fits into the build earlier in the epic levels -- however right now it looks like combat insight is better than other feats. The build then grabs invisible blade levels as the best damage class. Then it grabs Dervish levels as the next best damage class.

This showcases the short sighted thinking that drives the level warp algorithm. At each step, a short term goal cements the computers plan on how to build in the future. It's interesting that the strategies the computer takes are quite varied -- sometimes going for the highest damage build and sometimes going for bonus feats.
