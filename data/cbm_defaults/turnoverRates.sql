select 
    eco_boundary.id as EcoBoundaryID,
    turnover_parameter.sw_foliage as SoftwoodFoliageFallRate, 
    turnover_parameter.hw_foliage as HardwoodFoliageFallRate, 
    turnover_parameter.stem_turnover as StemAnnualTurnoverRate, 
    turnover_parameter.sw_branch as SoftwoodBranchTurnoverRate, 
    turnover_parameter.hw_branch as HardwoodBranchTurnoverRate,
    turnover_parameter.coarse_ag_split as CoarseRootAGSplit, 
    turnover_parameter.coarse_root as CoarseRootTurnProp, 
    turnover_parameter.fine_ag_split as FineRootAGSplit, 
    turnover_parameter.fine_root as FineRootTurnProp,
    turnover_parameter.branch_snag_split as OtherToBranchSnagSplit,
    turnover_parameter.branch_snag as BranchSnagTurnoverRate,
    turnover_parameter.stem_snag as StemSnagTurnoverRate
from turnover_parameter
inner join eco_boundary on eco_boundary.turnover_parameter_id = turnover_parameter.id
order by eco_boundary.id
