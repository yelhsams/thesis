use crate::parse_clif;
use std::fs;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_branch_display() {
        let clif_code =
            fs::read_to_string("test_branch_display.clif").expect("Failed to read test file");

        match parse_clif(&clif_code) {
            Ok((dfg, layout)) => {
                println!("Successfully parsed CLIF code\n");
                println!("Displaying instructions:\n");

                // Display all blocks and their instructions
                for &block_id in &layout.blocks {
                    let block = &layout.block_data[&block_id];

                    // Display block header with parameters
                    print!("block{}(", block_id.0);
                    for (i, &param) in block.params.iter().enumerate() {
                        if i > 0 {
                            print!(", ");
                        }
                        let ty = dfg.value_type(param);
                        print!("{}: {}", param, ty);
                    }
                    println!("):");

                    for &inst_id in &block.insts {
                        let display = dfg.display_inst(inst_id);
                        println!("    {}", display);
                    }
                    println!();
                }

                // Check specific instructions for branch targets
                println!("\nVerifying branch display:");
                for &block_id in &layout.blocks {
                    let block = &layout.block_data[&block_id];
                    for &inst_id in &block.insts {
                        let display = dfg.display_inst(inst_id);

                        if display.contains("brif") {
                            println!("✓ Found conditional branch: {}", display);
                            if display.contains("block1") && display.contains("block2") {
                                println!("  ✓ Both branch targets displayed");
                            } else {
                                println!("  ✗ Branch targets missing!");
                            }
                        }

                        if display.contains("jump") {
                            println!("✓ Found unconditional jump: {}", display);
                            if display.contains("block") {
                                println!("  ✓ Jump target displayed");
                            } else {
                                println!("  ✗ Jump target missing!");
                            }
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("Parse error: {}", e);
                std::process::exit(1);
            }
        }
    }
}
