export const nodeValues: Record<
    string,
    {
        prevalence: number;
        min: number;
        max: number;
        units: string;
        SD: number;
    }
> = {
    'ukb_b-8476': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-6519': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-5779': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-5238': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-5237': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-5076': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-4956': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-4710': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-3957': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ukb_b-19953': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ieu_a-961': {
        prevalence: 0.018,
        min: 0,
        max: 35,
        units: 'Cigs/day',
        SD: 11.7,
    },
    'ieu_a-7': {
        prevalence: 0.014,
        min: 0,
        max: 1,
        units: 'Odds (%)',
        SD: null,
    },
    'ieu_a-24': {
        prevalence: 0.056,
        min: 0,
        max: 1,
        units: 'Odds (%)',
        SD: null,
    },
    'ieu_a-1187': {
        prevalence: 0.04,
        min: 0,
        max: 1,
        units: 'Odds (%)',
        SD: null,
    },
    'ieu_a-118': {
        prevalence: 1.05,
        min: 0,
        max: 2.1,
        units: 'Arbitrary score',
        SD: 0.35,
    },
    'ieu_a-1018': { prevalence: 3, min: 0, max: 6, units: 'SD', SD: 1 },
    'ieu_a-1239': {
        prevalence: 16.8,
        min: 4.4,
        max: 28.4,
        units: 'Years',
        SD: 4.2,
    },

    // Unused traits
    'ukb_b-4424': { prevalence: 7.15, min: 1, max: 23, units: 'Hrs/d', SD: 1 },
    'ukb_d-SLEEP': {
        prevalence: 0.022,
        min: 0,
        max: 1,
        units: 'Odds (%)',
        SD: 1,
    },
};

export const isGood = {
    'ieu_a-1187': 'Bad', //Depression,
    'ukb_b-6519': 'Bad', //Worry,
    'ieu_a-1018': 'Good', //Wellbeing,
    'ukb_b-8476': 'Bad', //Loneliness,
    'ukb_b-3957': 'Bad', //Sleeplessness,
    'ukb_d-SLEEP': 'Bad', //ICD10 Insomni,
    'ukb_b-4062': 'Good', //Happiness,
    'ieu_a-118': 'Bad', //Neuroticism,
    'ukb_b-5779': 'Bad', //Alcohol,
    'ieu_a-1239': 'Good', //Education,
    'ukb_b-19953': 'Neutral', //BMI,
    'ukb_b-5238': 'Good', //Intelligence,
    'ukb_b-4956': 'Neutral', //Eveningness,
    'ukb_b-5076': 'Bad', //Not socialising,
    'ieu_a-961': 'Bad', //Smoking,
    'ukb_b-4710': 'Good', //Exercise,
    'ukb_b-5237': 'Neutral', //Coffee intake,
    'ieu_a-7': 'Bad', //CHD,
    'ukb_b-4424': 'Good', //Sleep duration,
    'ieu_a-24': 'Bad', //Diabetes
};
