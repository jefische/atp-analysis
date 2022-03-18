#' @title Build GBM Tree
#' @description Create a `data.tree` object from a GBM tree.
#' @param gbm_model Object of class `gbm`
#' @param i.tree Tree iteration to build from
#' @return A `data.tree` object from the `i.tree` tree of `gbm_model`.
build_tree <- function(gbm_model, i.tree = 1) {
  gbm_tree <- gbm::pretty.gbm.tree(gbm_model, i.tree = i.tree)
  pathString <- c("0" = "0")
  
  for (node in seq(1, nrow(gbm_tree) - 1)) {
    if (node %in% gbm_tree$MissingNode[gbm_tree$MissingNode != -1]) {
      temp_string <- NA
      # paste(
      #     pathString[
      #         which(
      #             names(pathString) == as.character(
      #                 which(gbm_tree$MissingNode == node) - 1
      #             )
      #         )
      #     ],
      #     paste("(M)", node),
      #     sep = "/"
      # )
    } else if (node %in% gbm_tree$LeftNode[gbm_tree$LeftNode != -1]) {
      temp_string <- paste(
        pathString[
          which(
            names(pathString) == as.character(
              which(gbm_tree$LeftNode == node) - 1
            )
          )
        ],
        paste("(L)", node),
        sep = "/"
      )
    } else if (node %in% gbm_tree$RightNode[gbm_tree$RightNode != -1]) {
      temp_string <- paste(
        pathString[
          which(
            names(pathString) == as.character(
              which(gbm_tree$RightNode == node) - 1
            )
          )
        ],
        paste("(R)", node),
        sep = "/"
      )
    }
    
    pathString <- append(pathString, temp_string)
    names(pathString) <- seq(0, length(pathString) - 1)
  }
  
  predictors <- gbm_model$var.names
  names(predictors) <- seq_len(length(predictors))
  gbm_tree$pathString <- unname(pathString)
  gbm_data_tree <- data.tree::as.Node(gbm_tree)
  
  # Plotting
  data.tree::SetGraphStyle(gbm_data_tree, rankdir = "LR", dpi = 70)
  
  data.tree::SetEdgeStyle(
    gbm_data_tree,
    fontname = "Palatino-italic",
    labelfloat = TRUE,
    fontsize = "26",
    label = function(node) {
      paste(
        dplyr::if_else(grepl("(L)", node$name, fixed = TRUE), "<", ">="),
        formatC(as.numeric(node$SplitCodePred), format = "f", digits = 6)
      )
    }
  )
  
  # Set node style for all of tree
  data.tree::SetNodeStyle(
    gbm_data_tree,
    fontsize = "26",
    fontname = function(node) dplyr::if_else(data.tree::isLeaf(node), "Palatino", "Palatino-bold"),
    height = "0.75",
    width = "1",
    shape = function(node) dplyr::if_else(
      data.tree::isLeaf(node),
      "box",
      "diamond"
    ),
    label = function(node) dplyr::case_when(
      data.tree::isLeaf(node) ~ paste("Prediction: ", formatC(as.numeric(node$Prediction), format = "f", digits = 6)), # For leaves
      node$SplitVar == -1 ~ as.character(unname(predictors[as.character(gbm_tree$SplitVar[1] + 1)])), # For root node
      TRUE ~ as.character(unname(predictors[as.character(node$SplitVar + 1)])) # For every other node
    )
  )
  
  gbm_data_tree
}