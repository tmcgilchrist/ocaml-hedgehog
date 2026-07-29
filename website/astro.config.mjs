import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

export default defineConfig({
  site: 'https://tmcgilchrist.github.io',
  base: '/ocaml-hedgehog',
  integrations: [
    starlight({
      title: 'Hedgehog',
      customCss: ['./src/styles/nord.css'],
      social: [
        { icon: 'github', label: 'GitHub', href: 'https://github.com/tmcgilchrist/ocaml-hedgehog' },
      ],
      sidebar: [
        { label: 'Getting Started', slug: 'guides/getting-started' },
        { label: 'Motivation', slug: 'guides/motivation' },
        { label: 'Tutorial', slug: 'guides/tutorial' },
        { label: 'State Testing', slug: 'guides/state-testing' },
        { label: 'Alternatives', slug: 'guides/alternatives' },
        { label: 'Resources', slug: 'guides/resources' },
        {
          label: 'API Reference',
          items: [
            { label: 'Gen', slug: 'api/gen' },
            { label: 'Property', slug: 'api/property' },
            { label: 'Range', slug: 'api/range' },
            { label: 'Tree', slug: 'api/tree' },
            { label: 'Shrink', slug: 'api/shrink' },
            { label: 'Seed', slug: 'api/seed' },
            { label: 'Stm', slug: 'api/stm' },
            { label: 'Diff', slug: 'api/diff' },
          ],
        },
      ],
    }),
  ],
});
