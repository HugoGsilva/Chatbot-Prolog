#!/usr/bin/env python3
"""
Script to validate the augmented intent data.

Checks:
1. JSON structure validity
2. Intent distribution
3. Example quality (no duplicates, minimum length)
4. Augmentation statistics
"""

import json
from pathlib import Path
from collections import Counter, defaultdict
from typing import Dict, List, Set


def load_augmented_data(filepath: str) -> Dict[str, List[str]]:
    """Load augmented intent examples."""
    with open(filepath, 'r', encoding='utf-8') as f:
        return json.load(f)


def load_original_data(filepath: str) -> Dict[str, List[str]]:
    """Load original intent patterns for comparison."""
    with open(filepath, 'r', encoding='utf-8') as f:
        data = json.load(f)
        
    # Extract unique examples from patterns
    intent_examples = defaultdict(set)
    for intent_data in data.get('intents', []):
        intent = intent_data.get('intent')
        patterns = intent_data.get('patterns', [])
        for pattern in patterns:
            intent_examples[intent].add(pattern.lower().strip())
    
    return {k: list(v) for k, v in intent_examples.items()}


def validate_structure(data: Dict[str, List[str]]) -> List[str]:
    """Validate JSON structure."""
    errors = []
    
    if not isinstance(data, dict):
        errors.append("❌ Root must be a dictionary")
        return errors
    
    for intent, examples in data.items():
        if not isinstance(intent, str):
            errors.append(f"❌ Intent key must be string: {intent}")
        
        if not isinstance(examples, list):
            errors.append(f"❌ Examples for '{intent}' must be a list")
            continue
        
        if len(examples) == 0:
            errors.append(f"⚠️  Intent '{intent}' has no examples")
        
        for i, example in enumerate(examples):
            if not isinstance(example, str):
                errors.append(f"❌ Example {i} in '{intent}' is not a string")
            elif len(example.strip()) < 3:
                errors.append(f"⚠️  Example '{example}' in '{intent}' is too short")
    
    return errors


def check_duplicates(data: Dict[str, List[str]]) -> Dict[str, List[str]]:
    """Find duplicate examples within each intent."""
    duplicates = {}
    
    for intent, examples in data.items():
        seen = set()
        dupes = []
        for example in examples:
            normalized = example.lower().strip()
            if normalized in seen:
                dupes.append(example)
            seen.add(normalized)
        
        if dupes:
            duplicates[intent] = dupes
    
    return duplicates


def check_cross_intent_duplicates(data: Dict[str, List[str]]) -> List[tuple]:
    """Find examples that appear in multiple intents."""
    example_to_intents = defaultdict(set)
    
    for intent, examples in data.items():
        for example in examples:
            normalized = example.lower().strip()
            example_to_intents[normalized].add(intent)
    
    # Find examples in multiple intents
    cross_duplicates = []
    for example, intents in example_to_intents.items():
        if len(intents) > 1:
            cross_duplicates.append((example, list(intents)))
    
    return cross_duplicates


def calculate_augmentation_ratio(
    original: Dict[str, List[str]], 
    augmented: Dict[str, List[str]]
) -> Dict[str, Dict]:
    """Calculate augmentation statistics."""
    stats = {}
    
    for intent in augmented.keys():
        orig_count = len(original.get(intent, []))
        aug_count = len(augmented[intent])
        ratio = aug_count / orig_count if orig_count > 0 else 0
        
        stats[intent] = {
            'original': orig_count,
            'augmented': aug_count,
            'ratio': ratio,
            'added': aug_count - orig_count
        }
    
    return stats


def print_statistics(stats: Dict[str, Dict]):
    """Print augmentation statistics."""
    print("\n📊 Augmentation Statistics")
    print("=" * 70)
    print(f"{'Intent':<30} {'Original':>10} {'Augmented':>10} {'Ratio':>8}")
    print("-" * 70)
    
    total_orig = 0
    total_aug = 0
    
    # Sort by ratio descending
    sorted_stats = sorted(stats.items(), key=lambda x: x[1]['ratio'], reverse=True)
    
    for intent, data in sorted_stats:
        total_orig += data['original']
        total_aug += data['augmented']
        
        ratio_str = f"{data['ratio']:.1f}x" if data['ratio'] > 0 else "N/A"
        print(f"{intent:<30} {data['original']:>10} {data['augmented']:>10} {ratio_str:>8}")
    
    print("-" * 70)
    overall_ratio = total_aug / total_orig if total_orig > 0 else float('inf')
    print(f"{'TOTAL':<30} {total_orig:>10} {total_aug:>10} {overall_ratio:>7.1f}x")
    print("=" * 70)


def analyze_example_lengths(data: Dict[str, List[str]]) -> Dict:
    """Analyze length distribution of examples."""
    all_lengths = []
    intent_lengths = {}
    
    for intent, examples in data.items():
        lengths = [len(ex.split()) for ex in examples]
        all_lengths.extend(lengths)
        intent_lengths[intent] = {
            'min': min(lengths) if lengths else 0,
            'max': max(lengths) if lengths else 0,
            'avg': sum(lengths) / len(lengths) if lengths else 0
        }
    
    return {
        'overall': {
            'min': min(all_lengths) if all_lengths else 0,
            'max': max(all_lengths) if all_lengths else 0,
            'avg': sum(all_lengths) / len(all_lengths) if all_lengths else 0
        },
        'by_intent': intent_lengths
    }


def main():
    """Run validation."""
    print("🔍 Validating Augmented Intent Data\n")
    
    # Paths
    augmented_path = Path(__file__).parent.parent / "app" / "intent_examples_augmented.json"
    original_path = Path(__file__).parent.parent / "app" / "intent_patterns.json"
    
    # Load data
    print(f"📂 Loading augmented data from: {augmented_path}")
    augmented_data = load_augmented_data(str(augmented_path))
    
    print(f"📂 Loading original data from: {original_path}")
    original_data = load_original_data(str(original_path))
    
    # 1. Validate structure
    print("\n1️⃣  Validating structure...")
    structure_errors = validate_structure(augmented_data)
    if structure_errors:
        print("❌ Structure validation failed:")
        for error in structure_errors:
            print(f"   {error}")
    else:
        print("✅ Structure is valid")
    
    # 2. Check duplicates
    print("\n2️⃣  Checking for duplicates...")
    duplicates = check_duplicates(augmented_data)
    if duplicates:
        print("⚠️  Found duplicate examples:")
        for intent, dupes in duplicates.items():
            print(f"   {intent}: {len(dupes)} duplicates")
            for dupe in dupes[:3]:  # Show first 3
                print(f"      - {dupe}")
    else:
        print("✅ No duplicate examples found")
    
    # 3. Check cross-intent duplicates
    print("\n3️⃣  Checking cross-intent duplicates...")
    cross_dupes = check_cross_intent_duplicates(augmented_data)
    if cross_dupes:
        print(f"⚠️  Found {len(cross_dupes)} examples in multiple intents:")
        for example, intents in cross_dupes[:5]:  # Show first 5
            print(f"   '{example}' in: {', '.join(intents)}")
    else:
        print("✅ No cross-intent duplicates found")
    
    # 4. Calculate augmentation ratio
    print("\n4️⃣  Calculating augmentation statistics...")
    stats = calculate_augmentation_ratio(original_data, augmented_data)
    print_statistics(stats)
    
    # 5. Analyze example lengths
    print("\n5️⃣  Analyzing example lengths...")
    length_stats = analyze_example_lengths(augmented_data)
    print(f"\n📏 Overall Length Statistics:")
    print(f"   Min: {length_stats['overall']['min']} words")
    print(f"   Max: {length_stats['overall']['max']} words")
    print(f"   Avg: {length_stats['overall']['avg']:.1f} words")
    
    # Show intents with very short/long examples
    print(f"\n   Intents with shortest avg length:")
    sorted_by_length = sorted(
        length_stats['by_intent'].items(), 
        key=lambda x: x[1]['avg']
    )
    for intent, lengths in sorted_by_length[:3]:
        print(f"      {intent}: {lengths['avg']:.1f} words")
    
    print(f"\n   Intents with longest avg length:")
    for intent, lengths in sorted_by_length[-3:]:
        print(f"      {intent}: {lengths['avg']:.1f} words")
    
    # Final summary
    print("\n" + "=" * 70)
    print("✅ Validation Complete!")
    
    total_intents = len(augmented_data)
    total_examples = sum(len(examples) for examples in augmented_data.values())
    
    print(f"📊 Summary:")
    print(f"   Total intents: {total_intents}")
    print(f"   Total examples: {total_examples}")
    print(f"   Avg examples per intent: {total_examples / total_intents:.1f}")
    
    total_orig_sum = sum(s['original'] for s in stats.values())
    total_aug_sum = sum(s['augmented'] for s in stats.values())
    if total_orig_sum > 0:
        print(f"   Overall augmentation: {total_aug_sum / total_orig_sum:.1f}x")
    else:
        print(f"   Note: Could not calculate augmentation ratio (original examples not found in intent_patterns.json)")
    
    if structure_errors or duplicates or cross_dupes:
        print(f"\n⚠️  Found {len(structure_errors) + len(duplicates) + len(cross_dupes)} issues")
        print("   Review warnings above before proceeding to Step 3")
    else:
        print("\n✅ All checks passed! Ready for Step 3.")
    
    print("=" * 70)


if __name__ == "__main__":
    main()
