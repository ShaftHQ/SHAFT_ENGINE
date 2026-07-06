#!/usr/bin/env python3
import re

# Read the file
with open('src/test/java/com/shaft/intellij/ui/ShaftPanelSetupTest.java', 'r') as f:
    lines = f.readlines()

# Find and replace the problematic lines
output = []
i = 0
while i < len(lines):
    line = lines[i]

    # Check for the start of the problem
    if 'JLabel assistantFamilyLabel = null;' in line:
        # Replace these lines:
        # Line i: JLabel assistantFamilyLabel = null;
        # Line i+1: JComboBox<?> familyCombo = null;
        # Line i+3: List<JLabel> foundLabels = new ArrayList<>();
        # Line i+4: List<JComponent> stepRowChildren = new ArrayList<>();
        output.append('        AtomicReference<JLabel> assistantFamilyLabel = new AtomicReference<>();\n')
        output.append('        AtomicReference<JComboBox<?>> familyCombo = new AtomicReference<>();\n')
        i += 2  # skip the two null declarations

        # skip the empty line
        if i < len(lines) and lines[i].strip() == '':
            output.append(lines[i])
            i += 1

        # skip the comment line
        if i < len(lines) and 'Walk the component tree' in lines[i]:
            output.append(lines[i])
            i += 1

        # skip and replace the List declarations
        if i < len(lines) and 'List<JLabel> foundLabels' in lines[i]:
            i += 1  # skip foundLabels line

        if i < len(lines) and 'List<JComponent> stepRowChildren' in lines[i]:
            output.append('        List<JPanel> childPanels = new ArrayList<>();\n')
            i += 1  # skip stepRowChildren line

    elif 'assistantFamilyLabel = lbl;' in line:
        output.append('                assistantFamilyLabel.set(lbl);\n')
        i += 1
    elif 'foundLabels.add(lbl);' in line:
        # Skip this line
        i += 1
    elif 'familyCombo = cmb;' in line:
        output.append('                familyCombo.set(cmb);\n')
        i += 1
    elif 'stepRowChildren.add((JComponent) comp);' in line:
        output.append('            if (comp instanceof JPanel pnl && comp != chooseRow) {\n')
        output.append('                childPanels.add(pnl);\n')
        output.append('            }\n')
        i += 1
    elif 'assertNotNull(assistantFamilyLabel,' in line:
        output.append('                () -> assertNotNull(assistantFamilyLabel.get(), "Should find \'Assistant family\' label"),\n')
        i += 1
    elif 'assertNotNull(familyCombo,' in line:
        output.append('                () -> assertNotNull(familyCombo.get(), "Should find family combobox"),\n')
        i += 1
    elif 'assertTrue(assistantFamilyLabel.getSize().width >=' in line:
        # Multi-line replacement
        output.append('                () -> assertTrue(assistantFamilyLabel.get().getSize().width >= assistantFamilyLabel.get().getPreferredSize().width,\n')
        i += 1
        if i < len(lines):
            output.append(lines[i].replace('assistantFamilyLabel.', 'assistantFamilyLabel.get().'))
            i += 1
        if i < len(lines):
            output.append(lines[i].replace('assistantFamilyLabel.', 'assistantFamilyLabel.get().'))
            i += 1
    elif 'assertTrue(assistantFamilyLabel.getSize().height >=' in line:
        output.append('                () -> assertTrue(assistantFamilyLabel.get().getSize().height >= assistantFamilyLabel.get().getPreferredSize().height,\n')
        i += 1
    elif 'for (JComponent child : stepRowChildren)' in line:
        output.append('        for (JPanel child : childPanels) {\n')
        i += 1
    elif 'if (child instanceof JPanel && child != chooseRow) {' in line:
        # This check is now redundant, skip it
        i += 1
        # Skip the block that checks the condition
        indent_count = 0
        while i < len(lines):
            if '}' in lines[i] and indent_count == 0:
                i += 1
                break
            if '{' in lines[i]:
                indent_count += 1
            output.append(lines[i])
            i += 1
    else:
        output.append(line)
        i += 1

# Write back
with open('src/test/java/com/shaft/intellij/ui/ShaftPanelSetupTest.java', 'w') as f:
    f.writelines(output)

print("Fixed!")
